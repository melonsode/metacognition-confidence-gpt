#-------------------------------------------------------------
# 欠損値がないかチェック
#-------------------------------------------------------------
library(readxl)

# 対象フォルダ
folder <- "1_raw_data/ChatGPT_API_n87"

# data-数字.xlsx のファイルだけ取得
files <- list.files(
  folder,
  pattern = "^ChatGPT-[0-9]{3}\\.xlsx$",
  full.names = TRUE
)

# チェック結果を保存するリスト
result <- list()
na_index_conf <- c()

for (f in files) {
  df <- read_excel(f)
  
  # 欠損があるかチェック
  na_conf <- any(is.na(df[["Confidence"]]))
  if(na_conf){
    print(paste( f, "Confidence") )
    print( which(is.na(df[["Confidence"]]) ) ) 
    na_index_conf <- c(na_index_conf, which(is.na(df[["Confidence"]]) ) )
  }
  na_corr <- any(is.na(df[["Correctness"]]))
  if(na_corr){
    print(paste( f, "Correctness") )
    print( which( is.na(df[["Correctness"]]) ) )
  }
  result[[f]] <- c(
    Confidence_NA = na_conf,
    Correctness_NA = na_corr
  )
}
na_index_conf

# 結果表示
result
# NA があるものだけ抽出
na_only <- result[ sapply(result, function(x) any(x)) ]

# 表示
if (length(na_only) == 0) {
  cat("Confidence / Correctness の欠損値は 0 件でした。\n")
} else {
  print(na_only)
}


#-------------------------------------------------------------
# 各質問ごとの平均正答率と確信度を計算
#-------------------------------------------------------------

corr_list <- list()
conf_list <- list()

#for (f in files_ex049) {
for (f in files) {
  df <- read_excel(f)
  
  id <- tools::file_path_sans_ext( basename(f) )  # 例: "ChatGPT-041"
  
  corr_list[[id]] <- df[["Correctness"]]
  conf_list[[id]] <- df[["Confidence"]]
}

# リスト → テーブル（列 = ファイル）
correctness_table <- as.data.frame(corr_list, check.names = FALSE)
confidence_table  <- as.data.frame(conf_list, check.names = FALSE)

n_agent <- ncol(correctness_table)

gpt_corr_df <- data.frame(
  Agent = rep( "GPT", 20 * 3 * n_agent ),
  AgentID = rep(1:n_agent, each = 20 * 3),
  Cond = rep( c(rep("op", 20), rep("4c", 20), rep("2c", 20)), times = n_agent),
  Qnum = rep( c(1:20, 1:20, 1:20), times = n_agent),
  Correctness = as.numeric( as.vector(as.matrix(correctness_table)) )
)

write.csv(gpt_corr_df, "1_raw_data/derived_data/gpt/correctness_long.csv", row.names = FALSE)


gpt_conf_df <- data.frame(
  Agent = rep( "GPT", 20 * 3 * n_agent ),
  AgentID = rep(1:n_agent, each = 20 * 3),
  Cond = rep( c(rep("op", 20), rep("4c", 20), rep("2c", 20)), times = n_agent),
  Qnum = rep( c(1:20, 1:20, 1:20), times = n_agent),
  Confidence = as.numeric( as.vector(as.matrix(confidence_table)) )
)

write.csv(gpt_conf_df, "1_raw_data/derived_data/gpt/confidence_long.csv", row.names = FALSE)

