library(ggplot2)
library(dplyr)

conf_corr_all_long <- read.csv("4_conf_correct_vs_incorrect/derived_data/all_correctness_confidence_long.csv")

fxp_df <- conf_corr_all_long %>%
  mutate(
    Cond = factor(Cond, 
                  levels = c("2C", "4C", "OP"), 
                  labels = c("2C", "4C", "OP") )
  ) %>%
  group_by(Agent, Cond, Qnum) %>%
  summarise(
    correct_rate = mean(Correctness),
    .groups = "drop"
  ) %>%
  mutate(correct_rate_adj = case_when(
    Cond == "2C" ~ (correct_rate - 0.5)  / (1 - 0.5),
    Cond == "4C" ~ (correct_rate - 0.25) / (1 - 0.25),
    Cond == "OP" ~ correct_rate
  )) %>%
  group_by(Agent) %>%
  mutate(
    # もともとのbin4
    #correct_rate_bin4 = ntile(correct_rate_adj, 4)
    
    # 改良bin4 uniqueな値を取り出してそれを４分割
    correct_rate_bin4 =
      {
        u <- sort(unique(correct_rate_adj))
        bin_u <- ntile(u, 4)
        bin_u[match(correct_rate_adj, u)]
      }
  ) %>%
  ungroup() %>%
  arrange(Agent, correct_rate_adj)


  


fxp_df %>%
  select(Agent, Cond, Qnum, correct_rate_bin4)


discriminability <- conf_corr_all_long %>%
  mutate(
    Cond = factor(Cond, 
                  levels = c("2C", "4C", "OP"), 
                  labels = c("2C", "4C", "OP") )
  ) %>%
  left_join(
    fxp_df %>%
      select(Agent, Cond, Qnum, correct_rate_bin4),
    by = c("Agent", "Cond", "Qnum")
  )

# おかしい値があるので手動で修正
# discriminability <- discriminability %>%
#   mutate(Confidence = case_when(
#     X == 1654 ~ 50,
#     X == 3027 ~ 70,
#     X == 4882 ~ 100,
#     TRUE ~ Confidence
#   ))


human_fxp <- discriminability %>%
  filter(Agent == "Human") %>%
  group_by(Correctness, correct_rate_bin4) %>%
  summarise(
      conf_mean = mean(Confidence),
      conf_median = median(Confidence),
      conf_sd = sd(Confidence),
      q1 = quantile(Confidence, 0.25),
      q3 = quantile(Confidence, 0.75),
      .groups = "drop"
    )

gpt_fxp <- discriminability %>%
  filter(Agent == "GPT") %>%
  group_by(Correctness, correct_rate_bin4) %>%
  summarise(
    conf_mean = mean(Confidence),
    conf_median = median(Confidence),
    conf_sd = sd(Confidence),
    q1 = quantile(Confidence, 0.25),
    q3 = quantile(Confidence, 0.75),
    .groups = "drop"
  )


library(colorspace)

# グラフ
fxp_graph <- function(data, agent_type){
  base_col <- if (agent_type == "Human") "#1f77b4" else "#ff7f0e"
  title_label <- if (agent_type == "Human") "Humans" else "GPT-4"
  
  col_map <- c(
    "Correct"   = base_col,
    "Incorrect" = desaturate(base_col, amount = 0.8)
  )
  
  lt_map <- c(
    "Correct"   = "solid",
    #"Incorrect" = "dashed"
    "Incorrect" = "longdash"
  )
  
  data <- data %>%
    mutate(Correctness = factor(Correctness, levels = c(1,0), labels = c("Correct", "Incorrect")  )  )
  p <- ggplot(data,
         aes(x = correct_rate_bin4,
             #y = conf_median,
             y = conf_mean,
             color = Correctness,
             linetype = Correctness,
             group = Correctness)) +
    ## 生データ（Confidence）を jitter
    #geom_jitter(
    #  data = discriminability %>% filter(Agent == agent_type) %>%
    #    mutate(Correctness = factor(Correctness, levels = c(1,0), labels = c("Correct", "Incorrect")  )  ),
    #  aes(
    #    x = correct_rate_bin4,
    #    y = Confidence,
    #    color = Correctness
    #  ),
    #  width = 0.15, height = 0, alpha = 0.3,size = 1
    #) +
    geom_line(linewidth = 1) +
    geom_point(size = 4) +
    #geom_errorbar(
    #  aes(ymin = q1, ymax = q3), 
      #aes(ymin = conf_mean - conf_sd, ymax = conf_mean + conf_sd), 
    #  width = 0.1) +
    coord_cartesian(ylim = c(0, 100)) +
    scale_color_manual(values = col_map, name = NULL) +
    scale_linetype_manual(values = lt_map, name = NULL) +
    labs(
      title = title_label,
      x = "Discriminability",
      y = "Confidence [%]",
      color = "Correctness"
    ) + 
    guides(color = guide_legend(title = NULL, nrow = 2)) +
    theme_minimal() +
    theme(
      legend.position = c(0.98, 0.02),
      legend.justification = c(1, 0),
      legend.direction = "horizontal",
      legend.background = element_rect(fill = NA, color = NA),
      legend.box.background = element_rect(fill = NA, color = NA),
      legend.key = element_rect(fill = NA, color = NA),
      plot.title = element_text(
        hjust = 0.5,          # センタリング
        color = "grey50",     # グレー（濃さは好みで調整）
        size  = 14            # 文字サイズ（任意）
      ),
      # 軸ラベルのフォントサイズ
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      
      # 軸目盛のフォントサイズ
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      
      # 凡例テキストのサイズ
      legend.text = element_text(size = 12),
      legend.key.width = unit(2.2, "lines")
    )
  
  return(p)
}

p_human <- fxp_graph(human_fxp, "Human")
p_human

p_gpt <- fxp_graph(gpt_fxp, "GPT")
p_gpt


# ggsave("7_folded-x/fig/folded_x_human_median_jitter.png", 
#        p_human, dpi = 300, width = 85, height = 85, units = "mm")
# ggsave("7_folded-x/fig/folded_x_gpt_median_jitter.png", 
#        p_gpt, dpi = 300, width = 85, height = 85, units = "mm")
# 
# 
# 
# ggsave("7_folded-x/fig/folded_x_human_median.tiff", 
#        p_human, dpi = 300, width = 85, height = 85, units = "mm")
# ggsave("7_folded-x/fig/folded_x_gpt_median.tiff", 
#        p_gpt, dpi = 300, width = 85, height = 85, units = "mm")
# 

#-------------------------------------------------------------
# グラフ結合
#-------------------------------------------------------------

library(patchwork)

p_combined <-  (p_human | p_gpt) +
  plot_annotation(tag_levels = "A")

p_combined

ggsave("7_folded-x/figures/mean_no_errorbar.png", 
       p_combined, dpi = 300, width = 180, units = "mm")

ggsave("7_folded-x/figures/tiff/mean_no_errorbar.tiff", 
       p_combined, dpi = 300, width = 180, units = "mm")



#-------------------------------------------------------------
# 各Discriminabilityの分布
#-------------------------------------------------------------
discriminability %>% 
  group_by(Agent, Correctness, correct_rate_bin4) %>%
  #filter(Agent == "GPT", Correctness == 0, correct_rate_bin4 == 2) %>% 
  select(Confidence)

plot_conf_hist <- function(discriminability, agent_type, incor_cor_b){
  print(agent_type)
  CorInc <- c("Incorrect", "Correct")
  discriminability %>%
    filter(Agent == agent_type, Correctness == incor_cor_b) %>%
    ggplot(aes(x = Confidence,
               color = factor(correct_rate_bin4),
               fill  = factor(correct_rate_bin4))) +
    geom_histogram(
      position = "identity",
      alpha = 0.3,
      bins = 20
    ) +
    coord_cartesian(ylim = c(1, 4000)) +
    scale_y_log10(limits = c(1, NA)) +
    labs(
      title = paste(agent_type, "-", CorInc[incor_cor_b + 1]),
      x = "Confidence",
      y = "Count",
      color = "Discriminability bin",
      fill  = "Discriminability bin"
    ) +
    theme_minimal() +
    theme(
      legend.position = c(0.05, 0.98),      # 左上（図内部）
      legend.justification = c(0, 1),       # 左上基準
      legend.background = element_rect(fill = NA, color = NA),
      legend.box.background = element_rect(fill = NA, color = NA),
      legend.key = element_rect(fill = NA, color = NA),
      legend.text = element_text(size = 8),     # ←文字を小さく
      legend.title = element_text(size = 8),    # ←タイトルも小さく
      legend.key.size = unit(0.4, "lines")      # ←キーを小さく
    ) + guides(
      color = guide_legend(title = NULL, nrow = 1),
      fill  = guide_legend(title = NULL, nrow = 1)
    )
}

p_H1 <- plot_conf_hist(discriminability, "Human", 1)
p_H1
p_H0 <- plot_conf_hist(discriminability, "Human", 0)
p_G1 <- plot_conf_hist(discriminability, "GPT", 1)
p_G1
p_G0 <- plot_conf_hist(discriminability, "GPT", 0)

p_combined <- (p_H1 | p_H0) / (p_G1 | p_G0) + plot_annotation(
  #title = "Confidence distributions",
  caption = "The y-axis is currently capped at 1000, but it should go up to 3000."
)
p_combined
ggsave("7_folded-x/fig/hist_discriminability_ylog.png", 
       p_combined, dpi = 300, width = 180, units = "mm")


