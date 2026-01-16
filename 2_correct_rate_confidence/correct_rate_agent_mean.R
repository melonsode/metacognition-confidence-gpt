library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)
library(emmeans)
library(afex)

# データの食い違い
# Human op 10 たぶん誤　0.72972973　正0.74712644　不明
# Human op 15 たぶん誤  0.38372093　正0.37931034　？のところ

# ----------------------------------------------
#  Human
# ----------------------------------------------
HUMANS_CORR_LONG <- read.csv("1_raw_data/derived_data/human/correctness_long.csv", stringsAsFactors = FALSE)

HUMANS_CORR_LONG <- HUMANS_CORR_LONG %>%
  mutate( 
    Cond = factor(Cond, levels = c("2c", "4c", "op"), labels = c("2C", "4C", "OP") )
  )

# Agent level mean
human_al_corr <- HUMANS_CORR_LONG %>% 
  group_by(Agent, Cond, AgentID) %>%
  summarise( Mean_CR = mean(Correctness), .groups = "drop" )

human_al_corr_stat <- human_al_corr %>% 
  group_by(Agent, Cond) %>%
  summarise( mean_al = mean(Mean_CR), sd_al = sd(Mean_CR), .groups = "drop")

# Question level mean
human_ql_corr <- HUMANS_CORR_LONG %>% 
  group_by(Agent, Cond, Qnum) %>%
  summarise( Mean_CR = mean(Correctness), .groups = "drop" )

human_ql_corr_stat <- human_ql_corr %>% 
  group_by(Agent, Cond) %>%
  summarise( mean_ql = mean(Mean_CR), sd_ql = sd(Mean_CR), .groups = "drop")



# ----------------------------------------------
# GPT 
# ----------------------------------------------
GPT_CORR_LONG <- read.csv("1_raw_data/derived_data/gpt/correctness_long.csv", stringsAsFactors = FALSE)
GPT_CORR_LONG <- GPT_CORR_LONG %>% 
  mutate( 
    Cond = factor(Cond, levels = c("2c", "4c", "op"), labels = c("2C", "4C", "OP") )
  )

# Agent level mean
gpt_al_corr <- GPT_CORR_LONG %>%
  group_by(Agent, Cond, AgentID) %>%
  summarise( Mean_CR = mean(Correctness), .groups = "drop" )

gpt_al_corr_stat <- gpt_al_corr %>% 
  group_by(Agent, Cond) %>%
  summarise( mean_al = mean(Mean_CR), sd_al = sd(Mean_CR), .groups = "drop")

# Question level mean
gpt_ql_corr <- GPT_CORR_LONG %>%
  group_by(Agent, Cond, Qnum) %>%
  summarise( Mean_CR = mean(Correctness), .groups = "drop" )

gpt_ql_corr_stat <- gpt_ql_corr %>% 
  group_by(Agent, Cond) %>%
  summarise( mean_ql = mean(Mean_CR), sd_ql = sd(Mean_CR), .groups = "drop")



# ----------------------------------------------
# 再利用用のデータ保存
# ----------------------------------------------
agent_mean_long <- rbind( human_al_corr, gpt_al_corr ) %>%
  write_csv("2_correct_rate_confidence/derived_data/agent_level/correct_rate_long.csv")

rbind( human_al_corr_stat, gpt_al_corr_stat ) %>%
  write_csv("2_correct_rate_confidence/derived_data/agent_level/correct_rate_mean_sd.csv")

rbind( human_ql_corr, gpt_ql_corr ) %>%
  write_csv("2_correct_rate_confidence/derived_data/question_level/correct_rate_long.csv")

rbind( human_ql_corr_stat, gpt_ql_corr_stat ) %>%
  write_csv("2_correct_rate_confidence/derived_data/question_level/correct_rate_mean_sd.csv")



#-------------------------------------------------
# Box bar graph
#-------------------------------------------------
g_ah <- ggplot(agent_mean_long %>% filter(Agent == "Human"), aes(x = Cond, y = Mean_CR)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.3, size = 1) +
  labs(x = "Condition", y = "Correct rate") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1)) +
  annotate("text", x = 2, y = 0.10, label = "Humans", size = 5)
g_ah

g_ag <- ggplot(agent_mean_long %>% filter(Agent == "GPT"), aes(x = Cond, y = Mean_CR)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.3, size = 1) +
  labs(x = "Condition", y = "Correct rate") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1)) +
  annotate("text", x = 2, y = 0.10, label = "GPT-4", size = 5)
g_ag

gcombined <- (g_ah | g_ag) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(plot.tag = element_text(size = 14, face = "bold"))
  ) +
  plot_annotation(
    title = "Agent mean",
    caption = ""
  )
gcombined
ggsave(
  "2_correct_rate_confidence/figures/correctrate_agent_mean.png",
  gcombined,
  width = 6,
  height = 3,
  dpi = 300
)

#-------------------------------------------------
# ANOVA
#-------------------------------------------------
test_data <- agent_mean_long %>%
  mutate(
    Agent = factor(Agent, levels = c("Human","GPT"), labels = c("Humans","GPT-4")),
    AgentID2 = interaction(Agent, AgentID, drop = TRUE)
  )


correct_rate_anova_output <- function(data, type){
  aov_result <- aov_ez(
    id = "AgentID2",
    dv = "Mean_CR",
    data = data,
    within = "Cond",
    between = "Agent",
    type = 3
  )
  
  txt_path <- paste0("2_correct_rate_confidence/derived_data/anova/pairs_", type, ".txt")  
  sink(txt_path)
  
  print(aov_result)
  anova_result_df <- as.data.frame(aov_result$anova_table)
  anova_result_df <- cbind(
    Term = rownames(anova_result_df),
    anova_result_df
  )
  rownames(anova_result_df) <- NULL
  print(anova_result_df)
  csv_path <- paste0("2_correct_rate_confidence/derived_data/anova/anova_", type, ".csv")  
  write_csv(anova_result_df, csv_path)
  
  
  # 多重比較
  # Cond（within）の主効果
  cat("\n\n## 多重比較 --- \n")
  cat("#Cond（within）の主効果\n")
  pairs_I <- pairs(emmeans(aov_result, ~ Cond), adjust = "bonferroni")
  print(pairs_I)
  cat("p-value: ", as.data.frame(pairs_I)[,"p.value"], "\n")
  # Agent（between）
  cat("\n#Agent（between）\n")
  pairs_I <- pairs(emmeans(aov_result, ~ Agent), adjust = "bonferroni")
  print(pairs_I)
  cat("p-value: ", as.data.frame(pairs_I)[,"p.value"], "\n")
  # 交互作用が有意なら（Agent 別に Cond）
  cat("\n#交互作用が有意なら（Agent 別に Cond））\n")
  pairs_I <- pairs(emmeans(aov_result, ~ Cond | Agent), adjust = "bonferroni")
  print(pairs_I)
  cat("p-value: ", as.data.frame(pairs_I)[,"p.value"], "\n")
  # Cond 別に Agent
  cat("\n# Cond 別に Agent\n")
  pairs_I <- pairs(emmeans(aov_result, ~ Agent | Cond), adjust = "bonferroni")
  print(pairs_I)
  cat("p-value: ", as.data.frame(pairs_I)[,"p.value"], "\n")
  sink()
}
correct_rate_anova_output(test_data, "correct_rate")
