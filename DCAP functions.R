library(readr)
library(tidyverse)

# rm(list = ls()) # to clean out workspace

CAP_original <- function(dataframe, key_variable, target_variable) {
  ### return the CAP score for an original dataset 
  key_freq <- plyr::count(dataframe, key_variable)
  target_key_freq <- plyr::count(dataframe, c(key_variable, target_variable))
  colnames(target_key_freq)[ncol(target_key_freq)] <- "tk_freq"
  colnames(key_freq)[ncol(key_freq)] <- "k_freq"
  dataframe %<>%
    inner_join(target_key_freq, by = grep("tk_freq", colnames(target_key_freq), value = TRUE, invert = TRUE)) %>%
    inner_join(key_freq, by =  grep("k_freq", colnames(key_freq), value = TRUE, invert = TRUE))
  dataframe$DCAP_original <- dataframe$tk_freq/dataframe$k_freq
  dcap_original <- mean(dataframe$DCAP_original)
  print(paste0("df_original mean DCAP = ", dcap_original))
}

CAP_baseline <- function(dataframe, target_variable) {
  ### return the baseline CAP score for an original dataset
  ### generate univariate distribution of target variables ###
  target_freq <- plyr::count(dataframe, target_variable)
  t_freq_total <- sum(target_freq$freq)
  target_freq$prob <- target_freq$freq/t_freq_total
  dcap_univariate <- mean(target_freq$prob)
  print(paste0("df_original baseline CAP = ", dcap_univariate))
}

CAP_synthetic <- function(dataframe, synthetic_dataframe, key_variable, target_variable){
  ### return the CAP score for a synthetic dataset 
  key_freq <- plyr::count(dataframe, key_variable)
  target_key_freq <- plyr::count(dataframe, c(key_variable, target_variable))
  colnames(target_key_freq)[ncol(target_key_freq)] <- "tk_freq"
  colnames(key_freq)[ncol(key_freq)] <- "k_freq"
  dataframe %<>%
    inner_join(target_key_freq, by = grep("tk_freq", colnames(target_key_freq), value = TRUE, invert = TRUE)) %>%
    inner_join(key_freq, by =  grep("k_freq", colnames(key_freq), value = TRUE, invert = TRUE))
  # dataframe$DCAP_original <- dataframe$tk_freq/dataframe$k_freq
  # dcap_original <- mean(dataframe$DCAP_original)
  
  key_freq_synt <- plyr::count(synthetic_dataframe, c(key_variable))
  colnames(key_freq_synt)[ncol(key_freq_synt)] <- "syn_key_freq"
  denominator <- inner_join(key_freq, key_freq_synt)
  denominator$dm_count <- pmin(denominator$k_freq, denominator$syn_key_freq)
  
  synth_target_key_freq <- plyr::count(synthetic_dataframe, c(key_variable, target_variable))
  colnames(synth_target_key_freq)[ncol(synth_target_key_freq)] <- "synth_tk_freq"
  numerator <- inner_join(synth_target_key_freq, target_key_freq)
  numerator$nm_count <- pmin(numerator$tk_freq, numerator$synth_tk_freq)
  
  df_synthetic <- left_join(dataframe, numerator)
  df_synthetic <- left_join(df_synthetic, denominator)  
  df_synthetic$DCAP_syn <- df_synthetic$nm_count / df_synthetic$dm_count
  
  dcap_synthetic1 <- mean(df_synthetic$DCAP_syn, na.rm=TRUE)
  print(paste0("df_synthetic mean DCAP when NAs are undefined = ", dcap_synthetic1))
  
  df_synthetic$DCAP_syn2 <- df_synthetic$DCAP_syn
  df_synthetic$DCAP_syn2[is.na(df_synthetic$DCAP_syn2)] <- 0
  dcap_synthetic2 <- mean(df_synthetic$DCAP_syn2)
  print(paste0("df_synthetic mean DCAP when NAs are 0 = ", dcap_synthetic2))
}

CAP_original_num <- function(dataframe, key_variable, target_variable, y = 2500) {
  ### return the CAP score for an original dataset where target_variable is numeric 
  dataframe[[target_variable]] <- round_any(as.numeric(dataframe[[target_variable]]), y) 
  key_freq <- plyr::count(dataframe, key_variable)
  target_key_freq <- plyr::count(dataframe, c(key_variable, target_variable))
  colnames(target_key_freq)[ncol(target_key_freq)] <- "tk_freq"
  colnames(key_freq)[ncol(key_freq)] <- "k_freq"
  dataframe %<>%
    inner_join(target_key_freq, by = grep("tk_freq", colnames(target_key_freq), value = TRUE, invert = TRUE)) %>%
    inner_join(key_freq, by =  grep("k_freq", colnames(key_freq), value = TRUE, invert = TRUE))
  dataframe$DCAP_original <- dataframe$tk_freq/dataframe$k_freq
  dcap_original <- mean(dataframe$DCAP_original)
  print(paste0("df_original mean DCAP = ", dcap_original))
}

CAP_synthetic_num <- function(dataframe, synthetic_dataframe, key_variable, target_variable, y = 2500){
  ### return the CAP score for a synthetic dataset 
  dataframe[[target_variable]] <- round_any(as.numeric(dataframe[[target_variable]]), y) 
  key_freq <- plyr::count(dataframe, key_variable)
  target_key_freq <- plyr::count(dataframe, c(key_variable, target_variable))
  colnames(target_key_freq)[ncol(target_key_freq)] <- "tk_freq"
  colnames(key_freq)[ncol(key_freq)] <- "k_freq"
  dataframe %<>%
    inner_join(target_key_freq, by = grep("tk_freq", colnames(target_key_freq), value = TRUE, invert = TRUE)) %>%
    inner_join(key_freq, by =  grep("k_freq", colnames(key_freq), value = TRUE, invert = TRUE))
  
  synthetic_dataframe[[target_variable]] <- round_any(as.numeric(synthetic_dataframe[[target_variable]]), y) 
  key_freq_synt <- plyr::count(synthetic_dataframe, c(key_variable))
  colnames(key_freq_synt)[ncol(key_freq_synt)] <- "syn_key_freq"
  denominator <- inner_join(key_freq, key_freq_synt)
  denominator$dm_count <- pmin(denominator$k_freq, denominator$syn_key_freq)
  
  synth_target_key_freq <- plyr::count(synthetic_dataframe, c(key_variable, target_variable))
  colnames(synth_target_key_freq)[ncol(synth_target_key_freq)] <- "synth_tk_freq"
  numerator <- inner_join(synth_target_key_freq, target_key_freq)
  numerator$nm_count <- pmin(numerator$tk_freq, numerator$synth_tk_freq)
  
  df_synthetic <- left_join(dataframe, numerator)
  df_synthetic <- left_join(df_synthetic, denominator)  
  df_synthetic$DCAP_syn <- df_synthetic$nm_count / df_synthetic$dm_count
  
  dcap_synthetic1 <- mean(df_synthetic$DCAP_syn, na.rm=TRUE)
  print(paste0("df_synthetic mean DCAP when NAs are undefined = ", dcap_synthetic1))
  
  df_synthetic$DCAP_syn2 <- df_synthetic$DCAP_syn
  df_synthetic$DCAP_syn2[is.na(df_synthetic$DCAP_syn2)] <- 0
  dcap_synthetic2 <- mean(df_synthetic$DCAP_syn2)
  print(paste0("df_synthetic mean DCAP when NAs are 0 = ", dcap_synthetic2))
}

CAP_baseline_num <- function(dataframe, target_variable, y = 2500) {
  ### return the baseline CAP score for an original dataset
  ### generate univariate distribution of target variables ###
  dataframe[[target_variable]] <- round_any(as.numeric(dataframe[[target_variable]]), y) 
  target_freq <- plyr::count(dataframe, target_variable)
  t_freq_total <- sum(target_freq$freq)
  target_freq$prob <- target_freq$freq/t_freq_total
  dcap_univariate <- mean(target_freq$prob)
  print(paste0("df_original baseline CAP = ", dcap_univariate))
}



### Testing Code 
# library(synthpop)
# df <- mtcars
# key_var <- c("cyl", "gear")
# target_var <- c("wt", "carb")
# 
# syn1 <- syn(df, seed = 1234)
# synthpop_df <- syn1$syn
# view(synthpop_df)
# 
# CAP_original(df, key_var, target_var)
# CAP_baseline(df, target_var)
# CAP_synthetic(df, synthpop_df, key_var, target_var)







