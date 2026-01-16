#install.packages("tidyverse")
#library(tidyr)
#library(purrr)
library(dplyr)
#library(readr)

# -------------------------------------------------------
# 書き出したCSVからmeta_Iの値を取得
# -------------------------------------------------------

n_start <- 1 # 1
n_agent <- 87 # 87

param <- expand.grid(
  c_type = 1:4,
  a_type = c("Human", "GPT"),
  id = n_start:n_agent
)
input_list <- vector("list", nrow(param))
for (k in seq_len(nrow(param))) {
  print(k)
  a_type <- param$a_type[k]
  id     <- param$id[k]
  c_type <- cond_label( cond_list[[ param$c_type[k] ]] )
  
  print(paste(a_type,id,c_type ))
  
  input_dir <- "6_meta_I/derived_data/meta_I_cutoff_sweep"
  input_dir <- file.path(input_dir, a_type, c_type)
  filename <- sprintf("%03d.csv", id)
  df <- readr::read_csv(file.path(input_dir, filename), show_col_types = FALSE)
  #print(df$meta_I.meta_I)

  meta_I_Ir2 <- data.frame(
    Agent = a_type,
    AgentID = id,
    Cond = c_type,
    meta_I = max(df$meta_I.meta_I),
    meta_Ir2 = max(df$meta_I.meta_Ir2)
  )
  #if(var(df$meta_I.meta_I) == 0){
  #  meta_I_Ir2$meta_I = NA
  #}
  #if(var(df$meta_I.meta_Ir2) == 0){
  #  meta_I_Ir2$meta_Ir2 = NA
  #}
  print(meta_I_Ir2)
  input_list[[k]] <- meta_I_Ir2
}

max_meta_I_table <- dplyr::bind_rows(input_list)

write_csv(max_meta_I_table, "6_meta_I/derived_data/max_meta_I_table.csv")

max_meta_I_table %>% 
  filter(meta_I == 0) %>%
  arrange(Cond)

