#install.packages("tidyverse")
#library(tidyr)
#library(purrr)
#library(dplyr)
#library(readr)
# ------------------------------------------------
# functions
# ------------------------------------------------
get_num_combination <- function(whole_data, cutoff, subj, cond){
  
  whole_data2 <- data.frame(
    Conf = whole_data$Conf,
    corIncor = whole_data$Judge,
    LowHight = whole_data$Conf
  )
  whole_data2$LowHigh <- ifelse(whole_data$Conf > cutoff, 1, 0)
  
  temp_h = subset(whole_data2, LowHigh == 1)
  temp_l = subset(whole_data2, LowHigh == 0)
  
  combi_result <- data.frame(
    Subject = subj,
    Condition = cond,
    All = nrow(whole_data2),
    High = nrow( temp_h ),
    Low = nrow( temp_l ),
    Incor = nrow( subset(whole_data2, corIncor == 0) ),
    Cor = nrow( subset(whole_data2, corIncor == 1) ),
    Low_Cor = nrow( subset(temp_l, corIncor == 1) ),
    High_Cor = nrow( subset(temp_h, corIncor == 1) )
  )
  print(combi_result)
  return(combi_result)
}
# entropy_factor : -p log(p)
entropy_factor_calc <- function(numerator, denominator){
  p = numerator / denominator
  if(numerator == 0){
    print("numerator is 0")
    return(0)
  }
  return( -1 * p * log(p, base = 2)  )
}
entropy_factor_calc_conditional <- function(num1, num2){
  p1 = num1 / (num1 + num2)
  p2 = num2 / (num1 + num2)
  calc1 = 0
  calc2 = 0
  if(num1 != 0){
    #print("num1 is not 0")
    calc1 = -1 * p1 * log(p1, base = 2)
  }else{
    print("num1 is 0")
  }
  if(num2 != 0){
    #print("num2 is not 0")
    calc2 = -1 * p2 * log(p2, base = 2)
  }else{
    print("num2 is 0")
  }
  return(  calc1 + calc2  )
}
calc_meta_I <- function(combi, subj, cond){
  c_all = combi$All
  c_h = combi$High
  c_h_c = combi$High_Cor
  c_h_ic = combi$High - combi$High_Cor
  c_l = combi$Low
  c_l_c = combi$Low_Cor
  c_l_ic = combi$Low - combi$Low_Cor
  c_c = combi$Cor
  c_ic = combi$All - combi$Cor
  
  #entropy_factor_calc 分子, 分母
  #entropy_factor_calc_conditional high low
  
  ent_H2_r = entropy_factor_calc(c_c ,c_all ) + entropy_factor_calc(c_ic, c_all)
  #print(c("H(r=0)+H(r=1)", ent_H2_r))
  ent_H2_c_h = entropy_factor_calc_conditional(c_h_c, c_h_ic)
  #print(ent_H2_c_h)
  ent_H2_c_l = entropy_factor_calc_conditional(c_l_c, c_l_ic)
  #print(ent_H2_c_l)
  meta_I = ent_H2_r - (c_h/c_all) * ent_H2_c_h - (c_l/c_all) * ent_H2_c_l
  #print( meta_I )
  #print(meta_I / ent_H2_r)
  
  meta_I_result <- data.frame(
    Subject = subj,
    Condition = cond,
    ent_H2_r = ent_H2_r,
    ent_H2_c_h = ent_H2_c_h,
    ent_H2_c_l = ent_H2_c_l,
    meta_I = meta_I,
    meta_Ir2 = meta_I / ent_H2_r
  )  
  print(meta_I_result)
  return(meta_I_result)
}
cond_label <- function(x) {
  if (length(x) == 1) x else "All"
}
sweep_cutoff <- function(data, agent, agent_id, condition){
  
  filtered <- data %>%
    filter(Agent == agent, AgentID == agent_id, Cond %in% condition  ) %>%
    select(Conf = Confidence, Judge = Correctness)
  
  meta_I_list <- list()
  
  for(i in 0:100){
    cutoff <- i
    combi <- get_num_combination(filtered, cutoff, agent, cond_label(condition))
    meta_I <- calc_meta_I(combi, agent, cond_label(condition))
    meta_I_list[[i + 1]] <- data.frame(
      cutoff = cutoff, combi = combi, meta_I = meta_I
    )
  }
  
  # 結果をcsvに保存
  df <- bind_rows(meta_I_list)
  out_dir <- "6_meta_I/derived_data/meta_I_cutoff_sweep/"
  out_dir <- paste0(out_dir, agent, "/", cond_label(condition), "/")
  filename <- paste0( sprintf("%03d", agent_id), ".csv" )
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  readr::write_csv(df, file.path(out_dir, filename ))
  
  # meta-Iをグラフ化したものを保存
  plot_data <- df %>% 
    select(cutoff , meta_I = meta_I.meta_I, meta_Ir2 = meta_I.meta_Ir2)
  
  g <- ggplot(plot_data, aes(x = cutoff)) +
    geom_line(aes(y = meta_I, color = "meta_I"), alpha = 0.5) +
    geom_line(aes(y = meta_Ir2, color = "meta_Ir2"), alpha = 0.5) +
    labs(color = "Metric") + 
    coord_cartesian(ylim = c(0, 0.75)) + 
    theme(
      legend.position = c(0.02, 0.98),
      legend.justification = c("left", "top")
    ) +
    annotate(
      "text",
      x = Inf, y = Inf,
      label = paste0(agent, ", ", cond_label(condition), "\nn=", agent_id ),
      hjust = 1.1, vjust = 1.1,
      size = 5
    )
  
  #message("plotting: ", agent, " id=", agent_id, " cond=", cond_label(condition))
  #grDevices::dev.new()
  #print(g)
  
  out_dir <- "6_meta_I/figures/meta_I_cutoff_sweep/"
  out_dir <- paste0(out_dir, agent, "/", cond_label(condition), "/")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  filename <- paste0( sprintf("%03d", agent_id), ".png" )
  ggsave(
    filename = file.path(out_dir, filename),
    plot = g,  width = 85,  height = 85,  units = "mm",dpi = 300
  )
  
  meta_I_Ir2 <- data.frame(
    Agent = agent,
    AgentID = agent_id,
    Cond = cond_label(condition),
    meta_I = max(df$meta_I.meta_I),
    meta_Ir2 = max(df$meta_I.meta_Ir2)
  )
  return(meta_I_Ir2)
  #return(df)
}

# ------------------------------------------------
# meat-I : Agent-level mean
# ------------------------------------------------
CORRECTNESS_CONF <- read.csv("4_conf_correct_vs_incorrect/derived_data/all_correctness_confidence_long.csv")


# -------------------------------------------------------
# {Human, GPT}, {2C,4C,OP,All}, {agent:1..87} の全条件でmeta_I算出
# 最大になるmeta_Iを取得
# -------------------------------------------------------
n_start <- 1 # 1
n_agent <- 87 # 87

max_meta_I_table <- data.frame(
  Agent = character(),
  AgentID = integer(),
  Cond = character(),
  max_meta_I = numeric(),
  max_meta_Ir2 = numeric()
)

param <- expand.grid(
  c_type = 1:4,
  a_type = c("Human", "GPT"),
  id = n_start:n_agent
)

cond_list <- list(c("2C"), c("4C"), c("OP"), c("2C", "4C","OP") )
out_list <- vector("list", nrow(param))

for (k in seq_len(nrow(param))) {
  c_type <- cond_list[[ param$c_type[k] ]]
  a_type <- param$a_type[k]
  id     <- param$id[k]
  print(paste(a_type,id,c_type ))
  
  out_list[[k]] <- sweep_cutoff(CORRECTNESS_CONF, a_type, id, c_type)
}

