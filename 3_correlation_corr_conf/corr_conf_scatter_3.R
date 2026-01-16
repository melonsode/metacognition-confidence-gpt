library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)

sink_flag = TRUE
#-------------------------------------------------------------
# Stimulus-level mean (Question-level mean)
#-------------------------------------------------------------
corr_rate_s <- read.csv("2_correct_rate_confidence/derived_data/question_level/correct_rate_long.csv", stringsAsFactors = FALSE)
conf_s <- read.csv("2_correct_rate_confidence/derived_data/question_level/confidence_long.csv", stringsAsFactors = FALSE)

merged_df_s <- full_join(
  corr_rate_s,
  conf_s,
  by = c("Agent", "Cond", "Qnum")
) %>%
  mutate(Agent = factor(Agent, c("Human", "GPT")))

write.csv(merged_df_s, "3_correlation_corr_conf/derived_data/correct_rate_conf/s_mean.csv")


#-------------------------------------------------------------
# Pearson相関係数 Stimulus-level mean
#-------------------------------------------------------------
calc_pearson <- function(merged_df, s_or_a ,sink_flag = FALSE){
  
  if (sink_flag) {
    print("sink flag: ON")
    filename <- paste0(s_or_a, "_mean.txt")
    sink(file.path("3_correlation_corr_conf/derived_data/pearson", filename))
    on.exit(sink(), add = TRUE)
  }
  
  cor_table <- merged_df %>%
    group_by(Agent) %>%
    summarise(
      cor = cor(Mean_CR, Mean_conf, use = "complete.obs"),
      p   = cor.test(Mean_CR, Mean_conf)$p.value,
      .groups = "drop"
    )

  print(paste0("## ", s_or_a, " mean"))
  for( agent in c("Human", "GPT") ){
    cr = merged_df %>% filter(Agent == agent) %>% pull(Mean_CR)
    cf = merged_df %>% filter(Agent == agent) %>% pull(Mean_conf)
    print(paste0("#", agent) )
    cor <- cor.test(cr,cf)
    print(cor )
    print("p.value --- ")
    print(cor$p.value )
    print("- - - - - -")
  }

}

calc_pearson(merged_df_q, "s", sink_flag)


#-------------------------------------------------------------
# 傾きが異なることの検定 Stimulus-level mean
#-------------------------------------------------------------
calc_lm <- function(merged_df, s_or_a, sink_flag = FALSE){
  
  human_average <- merged_df %>% filter(Agent == "Human")
  gpt_average <- merged_df %>% filter(Agent == "GPT")  

  if (sink_flag) {
    print("sink flag: ON")
    filename <- paste0(s_or_a, "_mean.txt")
    sink(file.path("3_correlation_corr_conf/derived_data/lm", filename))
    on.exit(sink(), add = TRUE)
  }
  
  print(paste0("## ", s_or_a, " mean"))
  print("# HumanとGPTでcorrect_rateとconfidenceの傾きが有意に異なる")
  fit <- lm(Mean_conf ~ Mean_CR * Agent, data = merged_df)
  print(summary(fit))
  print(summary(fit)[[4]][,4])
  
  print("# Humanでcorrect_rateとconfidenceの傾きが有意に0と異なる")
  fit_h <- lm(Mean_conf ~ Mean_CR, data = human_average)
  print(summary(fit_h))
  print(summary(fit_h)[[4]][,4])
  
  print("# GPTでcorrect_rateとconfidenceの傾きが有意に0と異なる")
  fit_g <- lm(Mean_conf ~ Mean_CR, data = gpt_average)
  print(summary(fit_g))
  print(summary(fit_g)[[4]][,4])
  
  return(list(human = fit_h$coefficients[["Mean_CR"]], 
              gpt = fit_g$coefficients[["Mean_CR"]] ))
}

beta_s <- calc_lm(merged_df_s, "s", sink_flag)

#-------------------------------------------------------------
# グラフ化
#-------------------------------------------------------------
plot_basic_scatter <- function(all_average, level){
  p <- ggplot(all_average, aes(x = Mean_CR, y = Mean_conf, color = Agent)) +
    # Humans（青丸・黒枠）
    geom_point(
      data = subset(all_average, Agent == "Human"),
      aes(fill = Agent),
      shape = 21,      # 21 = circle with fill + outline
      size = 2.7,
      #color = "#eee", # 外枠
      stroke = 0.3,     # 外枠の太さ
      alpha = 0.4      
    ) +
    # GPT-4（オレンジ三角・黒枠）
    geom_point(
      data = subset(all_average, Agent == "GPT"),
      aes(fill = Agent),
      shape = 24,      # 24 = triangle with fill + outline
      size = 2.7,
      #color = "#eee",
      stroke = 0.3,
      alpha = 0.4      
    ) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 100)) +
    # 色指定（必須）
    scale_fill_manual(
      values = c("Human" = "#1f77b4",    # blue
                 "GPT"  = "#ff7f0e"),    # orange
      labels = c("Humans", "GPT-4")
    ) +
    labs(
      title = paste0( level, "-level mean" ),
      x = "Correct rate",
      y = "Confidence [%]",
      fill = ""
    ) +
    theme_minimal() +
    # --- 凡例を右下に ---
    theme(
      plot.title = element_text(
        hjust = 0.5,          # センタリング
        color = "grey50",     # グレー（濃さは好みで調整）
        size  = 14            # 文字サイズ（任意）
      ),
      aspect.ratio = 1,
      legend.position = c(0.95, 0.05),   # (x, y) ＝右下
      legend.justification = c("right", "bottom"),
      
      # 軸ラベルのフォントサイズ
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      
      # 軸目盛のフォントサイズ
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      
      # 凡例テキストのサイズ
      legend.text = element_text(size = 12)
    )
}

p <- plot_basic_scatter(merged_df_s, "Stimulus")
p

add_lm_line <-function(p, all_average, posH, posG, beta){
  # 手動位置（ここだけ好きにいじる）
  label_pos <- tibble::tibble(
    Agent = factor(c("Human", "GPT") ),
    #x = c(0.07, 0.14),
    #y = c(25, 80)
    x = c(posH[1], posG[1]),
    y = c(posH[2], posG[2])
  )
  
  label_beta_q <- c( 
    paste0("italic(beta) == '", sprintf("%.1f", beta[1]), "'" ),
    paste0("italic(beta) == '", sprintf("%.1f", beta[2]), "'" )
  )
  
  p +
    geom_smooth(
      data = all_average,
      aes(x = Mean_CR, y = Mean_conf, color = Agent),
      method = "lm", se = FALSE, linewidth = 0.8 #, inherit.aes = FALSE,
    ) +
    geom_text(
      data = label_pos,
      aes(x = x, y = y, label = label_beta_q, color = Agent),
      inherit.aes = FALSE, hjust = 0, vjust = 1, size = 4, show.legend = FALSE, parse = TRUE
    ) +
    # geom_text(
    #   data = ann_df,
    #   aes(x = x, y = y, label = label_r, color = group),
    #   inherit.aes = FALSE, hjust = 0, vjust = 1, size = 4, show.legend = FALSE, parse = TRUE
    # ) +
    # geom_text(
    #   data = ann_df,
    #   aes(x = x, y = y - 6, label = label_p, color = group),
    #   inherit.aes = FALSE, parse = TRUE, hjust = 0, vjust = 1, size = 4, show.legend = FALSE
    # ) +
    scale_color_manual(
      values = c(
        "Human" = "#1f77b4",  # blue
        "GPT"  = "#ff7f0e"   # orange
      ), 
      labels = c("Humans", "GPT-4")
    ) +
    guides(
      color = guide_legend(
        title = "",
        override.aes = list(
          shape = c(21, 24),
          fill  = c("#1f77b4", "#ff7f0e"),
          #color = c("#eee", "#eee"),   # 凡例の枠色
          linetype = c(1, 1),          # 線も表示
          linewidth = 0.8
        )
      ),
      fill = "none"
    )
}

p2_q_mean <- add_lm_line(p, merged_df_s, 
                         c(0.07,25), c(0.14, 80),
                         c(beta_s$human, beta_s$gpt ) )
p2_q_mean

ggsave("3_correlation_corr_conf/figures/scatter_q_mean.png", 
       p2_q_mean, dpi = 300, width = 85, height = 85, units = "mm")
#ggsave("3_correlation_corr_conf/figures/tiff/scatter_q_mean.tiff", 
#       p2_q_mean, dpi = 300, width = 85, height = 85, units = "mm")


#-------------------------------------------------------------
#  Agent-level mean (Subject-level)
#-------------------------------------------------------------
corr_rate_a <- read.csv("2_correct_rate_confidence/derived_data/agent_level/correct_rate_long.csv", stringsAsFactors = FALSE)
conf_a <- read.csv("2_correct_rate_confidence/derived_data/agent_level/confidence_long.csv", stringsAsFactors = FALSE)

merged_df_a <- full_join(
  corr_rate_a,
  conf_a,
  by = c("Agent", "Cond", "AgentID")
) %>%
  mutate(Agent = factor(Agent, c("Human", "GPT")))

write.csv(merged_df_a, "3_correlation_corr_conf/derived_data/correct_rate_conf/a_mean.csv")


#-------------------------------------------------------------
# Pearson相関係数  Agent-level mean
#-------------------------------------------------------------
calc_pearson(merged_df_a, "a", sink_flag)

#-------------------------------------------------------------
# 傾きが異なることの検定  Agent-level mean
#-------------------------------------------------------------

beta_a <- calc_lm(merged_df_a, "a", sink_flag)


#-------------------------------------------------------------
# グラフ化
#-------------------------------------------------------------

p_a <- plot_basic_scatter(merged_df_a, "Agent")
p_a

p2_a_mean <- add_lm_line(p_a, merged_df_a, 
                         c(0.02,40), c(0.02, 95),
                         c(beta_a$human, beta_a$gpt ) )

p2_a_mean


ggsave("3_correlation_corr_conf/figures/scatter_a_mean.png", 
       p2_a_mean, dpi = 300, width = 85, height = 85, units = "mm")
#ggsave("3_correlation_corr_conf/figures/tiff/corr_conf_scatter_4_agent_mean.tiff", 
#       p2_a_mean, dpi = 300, width = 85, height = 85, units = "mm")


#-------------------------------------------------------------
# グラフ結合
#-------------------------------------------------------------
p_combined <-  (p2_q_mean | p2_a_mean) +
  plot_annotation(tag_levels = "A")

p_combined


ggsave("3_correlation_corr_conf/figures/scatter_combined.png", 
       p_combined, dpi = 300, width = 180, units = "mm")

ggsave("3_correlation_corr_conf/figures/tiff/scatter_combined.tiff", 
       p_combined, dpi = 300, width = 180, units = "mm")

