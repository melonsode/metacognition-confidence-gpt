#library(ARTool)
library(dplyr)
library(ggplot2)
library(patchwork)
#install.packages("afex")
library(afex)
library(readr)
library(emmeans)
#-------------------------------------------------------------
# Load data
#-------------------------------------------------------------
human_correctness <- read.csv("1_raw_data/derived_data/human/correctness_long.csv", stringsAsFactors = FALSE)
human_confidence <- read.csv("1_raw_data/derived_data/human/confidence_long.csv", stringsAsFactors = FALSE)

human_correctness <- cbind(
  human_correctness,
  Confidence = human_confidence$Confidence
)

gpt_correctness <- read.csv("1_raw_data/derived_data/gpt/correctness_long.csv", stringsAsFactors = FALSE)
gpt_confidence <- read.csv("1_raw_data/derived_data/gpt/confidence_long.csv", stringsAsFactors = FALSE)

gpt_correctness <- cbind(
  gpt_correctness,
  Confidence = gpt_confidence$Confidence
)

all_correctness <- rbind(
  human_correctness, gpt_correctness
)

all_correctness$Cond <-  factor(all_correctness$Cond, 
                                 levels = c("2c", "4c", "op"), 
                                 labels = c("2C", "4C", "OP"))


write.csv(all_correctness, "4_conf_correct_vs_incorrect/derived_data/all_correctness_confidence_long.csv")


# Agent AgentID Cond Qnum Correctness Confidence
# Human      87   4c   20           0         60
#  GPT       2   4c    7           1        100


#-------------------------------------------------------------
# Confidence for correct and incorrect responses
# Agent mean
# Bar graph
#-------------------------------------------------------------
plot_df_a <- all_correctness %>%
  group_by(Agent, AgentID, Correctness, Cond) %>%
  summarise(conf_mean = mean(Confidence), .groups = "drop") %>%
  group_by(Agent, Cond, Correctness) %>%
  summarise(
    mean = mean(conf_mean),
    sd   = sd(conf_mean),
    se   = sd(conf_mean) / sqrt(n()),
    n    = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Cond = factor(
      Cond,
      levels = c("2c", "4c", "op"),
      labels = c("2C", "4C", "OP")
    )
  ) %>%
  mutate(
    Correctness = factor(
      Correctness,
      levels = c(1, 0),
      labels = c("Correct", "Incorrect")
    )
  ) %>%
  mutate(
    Agent = factor(
      Agent,
      levels = c("Human", "GPT"),
      labels = c("Humans", "GPT-4")
    )
  )

box_bar_plot <- function(data, title){

  base_col <- if (title == "Humans") "#1f77b4" else "#ff7f0e"
  
  col_map <- c(
    "Correct"   = base_col,
    "Incorrect" = desaturate(base_col, amount = 0.8)
  )
  
  p <- ggplot(data, aes(x = Cond, y = mean, fill = Correctness)) +
    geom_col(position = position_dodge(width = 0.8)) +
    geom_errorbar(
      aes(ymin = mean - sd, ymax = mean + sd),
      width = 0.2,
      position = position_dodge(width = 0.8)
    ) +
    facet_grid(. ~ Correctness) +
    coord_cartesian(ylim = c(0, 100)) +
    scale_fill_manual(values = col_map, name = NULL) +
    labs(
      x = "Condition",
      y = "Confidence"
    ) +
    theme(legend.position = "none",
          # 軸ラベルのフォントサイズ
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          
          # 軸目盛のフォントサイズ
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          strip.text = element_text(size = 12)
    ) +
    labs(title = title) +
    theme(
      plot.title = element_text(
        hjust = 0.5,          # センタリング
        color = "grey50",     # グレー（濃さは好みで調整）
        size  = 14            # 文字サイズ（任意）
      )
    )
  
  return(p)
}

human_bar_a <- plot_df_a %>%
  filter(Agent == "Humans")
p_human_a <- box_bar_plot(human_bar_a, "Humans")
p_human_a
#ggsave("4_conf_correct_vs_incorrect/figures/agent_mean_human.png", 
#       p_human_a, dpi = 300, width = 85, height = 85, units = "mm")

gpt_bar_a <- plot_df_a %>%
  filter(Agent == "GPT-4")
p_gpt_a <- box_bar_plot(gpt_bar_a, "GPT-4")
p_gpt_a
#ggsave("4_conf_correct_vs_incorrect/figures/agent_mean_gpt.png", 
#       p_gpt_a, dpi = 300, width = 85, height = 85, units = "mm")


p_combined <-  (p_human_a | p_gpt_a) +
  plot_annotation(tag_levels = "A")

p_combined

ggsave("4_conf_correct_vs_incorrect/figures/agent_mean.tiff", 
       p_combined, dpi = 300, width = 180, units = "mm")
ggsave("4_conf_correct_vs_incorrect/figures/agent_mean.png", 
       p_combined, dpi = 300, width = 180, units = "mm")


#-------------------------------------------------------------
# means output
#-------------------------------------------------------------
plot_df_a_aligned <- plot_df_a %>%
  arrange(Agent, Correctness) %>%
  select(Agent, Correctness, Cond, everything())

write.csv( plot_df_a_aligned, "4_conf_correct_vs_incorrect/derived_data/mean_sd_se_agent_mean.csv")


#-------------------------------------------------------------
# ANOVA - Agent mean
# Agent       : GPT, Human
# Cond        : 2c,4c,op
# Correctness : Correct, Incorrect
#-------------------------------------------------------------
# 1) 集計 + 並び順固定（Humans→GPT-4 でまとまる）
anova_df <- all_correctness %>%
  group_by(Agent, Cond, Correctness, AgentID) %>%
  summarise(conf_mean = mean(Confidence), .groups = "drop") %>%
  mutate(
    Agent = factor(Agent, levels = c("Human","GPT"), labels = c("Humans","GPT-4")),
    #Cond  = factor(Cond,  levels = c("2c","4c","op"), labels = c("2C","4C","OP")),
    Correctness = factor(Correctness, levels = c(1,0), labels = c("Correct","Incorrect")),
    AgentID = factor(AgentID),
    AgentID2 = interaction(Agent, AgentID, drop = TRUE),
    grp = interaction(Agent, Cond, Correctness, sep=".", drop = TRUE)
  )


# 2) ANOVA（Type 3）
aov_res <- aov_ez(
  id = "AgentID2",
  dv = "conf_mean",
  data = anova_df,
  within = c("Correctness","Cond"),
  between = "Agent",
  type = 3
)
aov_res

# 3) ANOVA表を書き出し（項目名保持）
# 4) 多重比較
conf_anova_output <- function(aov_res){
  
  txt_path <- paste0("4_conf_correct_vs_incorrect/derived_data/anova_pairs.txt")  
  sink(txt_path)

  print(aov_metaI)
  anova_res_df <- as.data.frame(aov_res$anova_table)
  anova_res_df <- cbind(
    Term = rownames(anova_res_df),
    anova_res_df
  )
  rownames(anova_res_df) <- NULL
  csv_path <- paste0("4_conf_correct_vs_incorrect/derived_data/anova_.csv")  
  write_csv(anova_res_df, csv_path)

  cat("\n\n## 多重比較 --- \n")
  cat("# A) 全セル（Agent×Cond×Correctness）間の比較\n")
  # A) 全セル（Agent×Cond×Correctness）間の比較（pairwise.t.testの置き換えに一番近い）
  emm_all <- emmeans(aov_res, ~ Agent * Cond * Correctness)
  pairs <- pairs(emm_all, adjust = "bonferroni")
  print(pairs)
  cat("p-value: ", as.data.frame(pairs)[,"p.value"], "\n")

  cat("\n\n# B) Agent差を Cond×Correctness ごとに（Humans vs GPT-4 を各セルで）\n")
  # B) Agent差を Cond×Correctness ごとに（Humans vs GPT-4 を各セルで）
  emm_agent_in_cells <- emmeans(aov_res, ~ Agent | Cond * Correctness)
  pairs <- pairs(emm_agent_in_cells, adjust = "bonferroni")
  print(pairs)
  cat("p-value: ", as.data.frame(pairs)[,"p.value"], "\n")
  
  cat("\n\n# C) Cond差を Agent×Correctness ごとに（条件差を各グループで）\n")
  # C) Cond差を Agent×Correctness ごとに（条件差を各グループで）
  emm_cond_in_groups <- emmeans(aov_res, ~ Cond | Agent * Correctness)
  pairs <- pairs(emm_cond_in_groups, adjust = "bonferroni")
  print(pairs)
  cat("p-value: ", as.data.frame(pairs)[,"p.value"], "\n")
  
  cat("\n\n# D) Correctness差を Agent×Cond ごとに（Correct vs Incorrect）\n")
  # D) Correctness差を Agent×Cond ごとに（Correct vs Incorrect）
  emm_corr_in_groups <- emmeans(aov_res, ~ Correctness | Agent * Cond)
  pairs <- pairs(emm_corr_in_groups, adjust = "bonferroni")
  print(pairs)
  cat("p-value: ", as.data.frame(pairs)[,"p.value"], "\n")
  
  sink()
}
conf_anova_output(aov_res)
