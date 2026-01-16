library(openxlsx)
library(dplyr)

# ------------------------------------
# Humans raw data (n=87) : Correctness
# ------------------------------------

HUMAN_RAW <- read.xlsx("1_raw_data/Humans/humanresponses.xlsx", sheet="Sheet1")

cor_idx <- which(colnames(HUMAN_RAW) == "1/0.(correct/not)")
corr <- HUMAN_RAW[, cor_idx, drop = FALSE]
colnames(corr) <- c(rep("op", 20), rep("2c", 20), rep("4c", 20))
corr <- corr[-c(1, 89), ]

# Correct '?' judgments to 0
# 判定が？になっているので 0 に是正
corr[50,15] <- 0

n_subj <- nrow(corr)

human_corr_df <- data.frame(
  Agent = rep( "Human", 20 * 3 * n_subj ),
  AgentID = rep(1:n_subj, each = 20 * 3),
  Cond = rep( c(rep("op", 20), rep("2c", 20), rep("4c", 20)), times = n_subj),
  Qnum = rep( c(1:20, 1:20, 1:20), times = n_subj),
  Correctness = as.numeric( as.vector(t(corr)) )
)

write.csv(human_corr_df, "1_raw_data/derived_data/human/correctness_long.csv", row.names = FALSE)


# ------------------------------------
# Humans raw data (n=87) : Confidence from raw data
# ------------------------------------
HUMAN_FILE <- "1_raw_data/Humans/human_confidence_raw_manual_fixed.xlsx"
HUMAN_CONF <- read.xlsx(HUMAN_FILE, sheet="Sheet1", startRow = 1, colNames = TRUE)

human_conf_df <- data.frame(
  Agent = rep( "Human", 20 * 3 * n_subj ),
  AgentID = rep(1:n_subj, each = 20 * 3),
  Cond = rep( c(rep("op", 20), rep("2c", 20), rep("4c", 20)), times = n_subj),
  Qnum = rep( c(1:20), times = 3 * n_subj ),
  Confidence = as.numeric( as.vector(  as.matrix(HUMAN_CONF[1:60,4:90]) )  )
)
human_conf_df$row <- seq_len(nrow(human_conf_df))

# おかしい値があるので手動で修正
human_conf_df <- human_conf_df %>%
  mutate(Confidence = case_when(
    row == 3820 ~ 100,
    row == 1654 ~ 50,
    row == 3027 ~ 70,
    row == 4882 ~ 100,
    TRUE ~ Confidence
  ))


write.csv(human_conf_df, "1_raw_data/derived_data/human/confidence_long.csv", row.names = FALSE)
