#install.packages("tidyverse")
#library(tidyr)
#library(purrr)
library(dplyr)
library(readr)
library(emmeans)
library(afex)
library(tidyr)
library(ggplot2)


max_meta_I_table <- read_csv("6_meta_I/derived_data/max_meta_I_table.csv")



# -------------------------------------------------------
# meta_Iのヒストグラム
# -------------------------------------------------------

breaks <- seq(0, 0.8, by = 0.05)
g_hist <- ggplot( max_meta_I_table %>%
         filter(Agent == "Human")
                , aes(x = meta_I, fill = Cond)) +
  geom_histogram(
    #bins = 30,
    breaks = breaks,
    alpha = 0.4,
    position = "identity"
  ) +
  coord_cartesian(ylim=c(0,50)) +
  labs(
    title = "Human",
    x = "meta-I",
    y = "Count",
    fill = "Condition"
  ) +
  ggplot( max_meta_I_table %>%
          filter(Agent == "GPT")
        , aes(x = meta_I, fill = Cond)) +
  geom_histogram(
    #bins = 30,
    breaks = breaks,
    alpha = 0.4,
    position = "identity"
  ) +
  coord_cartesian(ylim=c(0,50)) +
  labs(
    title = "GPT",
    x = "max meta-I",
    y = "Count",
    fill = "Condition"
  )
g_hist
#ggsave(
#  filename = file.path("6_meta_I/figures", "meta_I_histogram.png"),
#  plot = g_hist,  width = 180,  height = 85,  units = "mm",dpi = 300
#)


# -------------------------------------------------------
# meta_Iを集計
# -------------------------------------------------------

# meta-I=0の確認
max_meta_I_table %>%
  #filter(is.na(meta_Ir2)) %>%
  filter(meta_I == 0 | is.na(meta_I)) %>%
  arrange(Cond)

meta_I_mean <- max_meta_I_table %>% 
  mutate(
    Cond = factor(Cond, levels = c("All", "2C", "4C", "OP")),
    Agent = factor(Agent, levels = c("Human", "GPT"))
    ) %>%
  arrange(Cond) %>%
  group_by(Agent, Cond) %>%
  summarise(
    n = sum(!is.na(meta_I)),
    n2 = sum(!is.na(meta_Ir2)),
    mI_mean = mean(meta_I, na.rm = TRUE),
    mI_sd = sd(meta_I, na.rm = TRUE),
    #mI_median = median(meta_I, na.rm = TRUE),
    mIr2_mean = mean(meta_Ir2, na.rm = TRUE),
    #mIr2_median = median(meta_Ir2, na.rm = TRUE),
    mIr2_sd = sd(meta_Ir2, na.rm = TRUE),
    .groups = "drop"
  )

#write_csv(meta_I_mean, "6_meta_I/derived_data/meta_I_mean.csv")

mIm <- meta_I_mean %>%
  select(Agent, Cond, mI_mean, mIr2_mean) %>%
  pivot_wider(
    names_from  = Agent,
    values_from = c(mI_mean, mIr2_mean)
  )

#mImd <- meta_I_mean %>%
#  select(Agent, Cond, mI_median, mIr2_median) %>%
#  pivot_wider(
#    names_from  = Agent,
#    values_from = c(mI_median, mIr2_median)
#  )

# -------------------------------------------------------
# meta_Iを条件間で可視化
# -------------------------------------------------------

p_metaI <- ggplot(max_meta_I_table %>%
        mutate(
          Cond = factor(Cond, levels = c("All", "2C", "4C", "OP")),
          Agent = factor(Agent, levels = c("Human", "GPT"), label = c("Humans", "GPT-4") )
        ) %>%
        arrange(Cond),
        aes(x = Cond, y = meta_I, fill = Agent)) +
  geom_boxplot(na.rm = TRUE, outlier.shape = NA) +
  geom_jitter(
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75),
    alpha = 0.4,
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
    title = "meta-I",
    x = "Condition",
    y = "meta-I"
  ) +
  theme_minimal() + 
  theme(
    legend.title = element_blank(),
    legend.position = c(0.2, 0.75),   # 内側・下
    legend.justification = c(0.5, 0),
    legend.background = element_rect(fill = "white", color = NA)
  )
p_metaI

#ggsave(
#  filename = file.path("6_meta_I/figures", "boxbar_meta_I.png"),
#  plot = p_metaI,  width = 100,  height = 85,  units = "mm",dpi = 300
#)

p_metaIr2 <- ggplot(max_meta_I_table %>%
                mutate(Cond = factor(Cond, levels = c("All", "2C", "4C", "OP")),
                Agent = factor(Agent, levels = c("Human", "GPT"), label = c("Humans", "GPT-4") )
              ) %>%
              arrange(Cond),
              aes(x = Cond, y = meta_Ir2, fill = Agent)) +
  geom_boxplot(na.rm = TRUE, outlier.shape = NA) +
  geom_jitter(
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75),
    alpha = 0.4,
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
    title = "meta-Ir2",
    x = "Condition",
    y = "meta-Ir2"
  ) +
  theme_classic() + 
  theme(
    legend.title = element_blank(),
    legend.position = c(0.2, 0.75),   # 内側・下
    legend.justification = c(0.5, 0),
    legend.background = element_rect(fill = scales::alpha("white", 0.2), color = NA)
  )
p_metaIr2

#ggsave(
#  filename = file.path("6_meta_I/fig", "boxbar_meta_Ir2.png"),
#  plot = p_metaIr2,  width = 100,  height = 85,  units = "mm",dpi = 300
#)



# ------------------------------------------------
# meta-I values were analyzed using a two-way ANOVA 
# with Agent (Human, GPT-4) and Condition (2C, 4C, OP) 
# as between-subjects factors.
# ------------------------------------------------

# ------------------------------------------------
# meta-I
# ------------------------------------------------
meta_I_test <- max_meta_I_table %>%
  filter(Cond %in% c("2C", "4C", "OP")) %>%
  mutate(
    Agent = factor(Agent, levels = c("Human","GPT"), labels = c("Humans","GPT-4")),
    Cond  = factor(Cond, levels = c("2C","4C","OP")),
    AgentID2 = interaction(Agent, AgentID, drop = TRUE)
  )

meta_I_anova_output <- function(data, meta_I_type){
  aov_metaI <- aov_ez(
    id = "AgentID2",
    dv = meta_I_type,
    data = data,
    within = "Cond",
    between = "Agent",
    type = 3
  )
  
  txt_path <- paste0("6_meta_I/derived_data/anova_pairs_", meta_I_type, ".txt")  
  sink(txt_path)
  
  print(aov_metaI)
  anova_metaI_df <- as.data.frame(aov_metaI$anova_table)
  anova_metaI_df <- cbind(
    Term = rownames(anova_metaI_df),
    anova_metaI_df
  )
  rownames(anova_metaI_df) <- NULL
  csv_path <- paste0("6_meta_I/derived_data/anova_", meta_I_type, ".csv")  
  write_csv(anova_metaI_df, csv_path)
  
  # 多重比較
  # Cond（within）の主効果
  cat("\n\n## 多重比較 --- \n")
  cat("#Cond（within）の主効果\n")
  pairs_I <- pairs(emmeans(aov_metaI, ~ Cond), adjust = "bonferroni")
  print(pairs_I)
  cat("p-value: ", as.data.frame(pairs_I)[,"p.value"], "\n")
  # Agent（between）
  cat("\n#Agent（between）\n")
  pairs_I <- pairs(emmeans(aov_metaI, ~ Agent), adjust = "bonferroni")
  print(pairs_I)
  cat("p-value: ", as.data.frame(pairs_I)[,"p.value"], "\n")
  # 交互作用が有意なら（Agent 別に Cond）
  cat("\n#交互作用が有意なら（Agent 別に Cond））\n")
  pairs_I <- pairs(emmeans(aov_metaI, ~ Cond | Agent), adjust = "bonferroni")
  print(pairs_I)
  cat("p-value: ", as.data.frame(pairs_I)[,"p.value"], "\n")
  # Cond 別に Agent
  cat("\n# Cond 別に Agent\n")
  pairs_I <- pairs(emmeans(aov_metaI, ~ Agent | Cond), adjust = "bonferroni")
  print(pairs_I)
  cat("p-value: ", as.data.frame(pairs_I)[,"p.value"], "\n")
  sink()
}


meta_I_anova_output(meta_I_test, "meta_I")
meta_I_anova_output(meta_I_test, "meta_Ir2")

