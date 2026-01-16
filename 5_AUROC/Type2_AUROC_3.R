library(readr)
library(dplyr)
#install.packages('Epi')
library('Epi')
library(ggplot2)
library(emmeans)
library(afex)


corr_conf <- read_csv("4_conf_correct_vs_incorrect/derived_data/all_correctness_confidence_long.csv")


calc_auc_by_agent <- function(df, agent_value, cond_value) {
  df %>%
    filter(Agent == agent_value, Cond == cond_value) %>%
    group_by(AgentID) %>%
    group_modify(~{
      temp <- .x %>%
        group_by(Correctness, Cond, Qnum) %>%
        summarise(conf_mean = mean(Confidence, na.rm = TRUE), .groups = "drop") %>%
        filter(!is.na(conf_mean)) %>%
        mutate(Correctness = as.integer(Correctness))
      
      if (dplyr::n_distinct(temp$Correctness) < 2 || var(temp$conf_mean) == 0) {
        return(tibble(AUC = NA_real_))
      }
      
      roc <- ROC(test = temp$conf_mean, stat = temp$Correctness, plot = "n")
      tibble(AUC = roc$AUC)
    }) %>%
    ungroup() %>%
    mutate(Agent = agent_value, Cond = cond_value) %>%
    select(Agent, Cond, AgentID, AUC)
}

auc_all <- bind_rows(
  calc_auc_by_agent(corr_conf, "Human", "2C"),
  calc_auc_by_agent(corr_conf, "Human", "4C"),
  calc_auc_by_agent(corr_conf, "Human", "OP"),
  calc_auc_by_agent(corr_conf, "GPT",   "2C"),
  calc_auc_by_agent(corr_conf, "GPT",   "4C"),
  calc_auc_by_agent(corr_conf, "GPT",   "OP")
)

auc_all <- bind_rows(
  auc_all,
  auc_all %>%
    filter(Cond %in% c("2C", "4C", "OP")) %>%
    mutate(Cond = "ALL")
) %>%
  mutate(Cond = factor(Cond, levels = c("ALL", "2C", "4C", "OP"))) %>%
  mutate(Agent = factor(Agent, levels = c("Human", "GPT"), label = c("Humans", "GPT-4") )) %>%
  arrange(Agent, Cond)


auc_all_mean <- auc_all　%>%
  group_by(Agent, Cond) %>%
  summarise(
    n = sum(!is.na(AUC)),
    AUC_mean = mean(AUC, na.rm = TRUE),
    AUC_sd   = sd(AUC, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  #mutate(Agent = factor(Agent, levels = c("Humans", "GPT-4"))) %>%
  mutate(Cond = factor(Cond, levels = c("ALL", "2C", "4C", "OP"))) %>%
  #mutate(Agent = factor(Agent, levels = c("Human", "GPT"), label = c("Humans", "GPT-4") )) %>%
  arrange(Agent, Cond)

write_csv(auc_all_mean, "5_AUROC/derived_data/mean_sd.csv")



p_AUROC <- ggplot(auc_all, aes(x = Cond, y = AUC, fill = Agent)) +
  geom_boxplot(na.rm = TRUE, outlier.shape = NA) +
  geom_jitter(
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75),
    alpha = 0.25,
    size = 1,
    na.rm = TRUE
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_fill_manual(
    values = c(
      "Humans" = "#1f77b4",  # 青
      "GPT-4"   = "#ff7f0e"   # オレンジ
    )
  ) +
  labs(
    x = "Condition",
    y = "AUROC2"
  ) +
  theme_classic() + 
  theme(
    legend.title = element_blank(),
    legend.position = c(0.8, 0.02),   # 内側・下
    legend.justification = c(0.5, 0),
    legend.background = element_rect(fill = "white", color = NA)
  )
p_AUROC
#ggsave("5_AUROC/figures/AUROC_box_bar_agent-level.png", 
#       p_AUROC, dpi = 300, width = 85, height = 85, units = "mm")




# ------------------------------------------------
# AUROC values were analyzed using a two-way ANOVA 
# with Agent (Human, GPT-4) and Condition (2C, 4C, OP) 
# as between-subjects factors.
# ------------------------------------------------

auc_test <- auc_all %>%
  filter(Cond %in% c("2C", "4C", "OP")) %>%
  mutate(
    AgentID2 = interaction(Agent, AgentID, drop = TRUE)
  )


auc_anova_output <- function(data, sink_flag){
  
  aov_auc <- aov_ez(
    id = "AgentID2",
    dv = "AUC",
    data = data,
    within = "Cond",
    between = "Agent",
    type = 3
  )
  
  if (sink_flag) {
    print("sink flag: ON")
    filename <- "anova_pairs.txt"
    sink(file.path("5_AUROC/derived_data", filename))
    on.exit(sink(), add = TRUE)
  }
  
  print(aov_auc)
  print("p.value - - - - ")
  #print(str(aov_auc))
  print( aov_auc$anova_table$`Pr(>F)` )
  print(" - - - - ")
  anova_auc_df <- as.data.frame(aov_auc$anova_table)
  anova_auc_df <- cbind(
    Term = rownames(anova_auc_df),
    anova_auc_df
  )
  rownames(anova_auc_df) <- NULL
  csv_path <- paste0("5_AUROC/derived_data/anova_", "auc", ".csv")  
  write_csv(anova_auc_df, csv_path)
  
  # 多重比較
  # Cond（within）の主効果
  cat("\n\n## 多重比較 --- \n")
  cat("#Cond（within）の主効果\n")
  pairs_I <- pairs(emmeans(aov_auc, ~ Cond), adjust = "bonferroni")
  print(pairs_I)
  cat("p-value: ", as.data.frame(pairs_I)[,"p.value"], "\n")
  # Agent（between）
  cat("\n#Agent（between）\n")
  pairs_I <- pairs(emmeans(aov_auc, ~ Agent), adjust = "bonferroni")
  print(pairs_I)
  cat("p-value: ", as.data.frame(pairs_I)[,"p.value"], "\n")
  # 交互作用が有意なら（Agent 別に Cond）
  cat("\n#交互作用が有意なら（Agent 別に Cond））\n")
  pairs_I <- pairs(emmeans(aov_auc, ~ Cond | Agent), adjust = "bonferroni")
  print(pairs_I)
  cat("p-value: ", as.data.frame(pairs_I)[,"p.value"], "\n")
  # Cond 別に Agent
  cat("\n# Cond 別に Agent\n")
  pairs_I <- pairs(emmeans(aov_auc, ~ Agent | Cond), adjust = "bonferroni")
  print(pairs_I)
  cat("p-value: ", as.data.frame(pairs_I)[,"p.value"], "\n")
}


auc_anova_output(auc_test, TRUE)
