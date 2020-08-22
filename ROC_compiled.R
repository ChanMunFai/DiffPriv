library(tidyverse)
library(readr)
library(dplyr)
library(janitor)

### Function 
ROC_score <- function (data1, data2, num_cat = 2000, min_items = 5){
  ### Read in 2 dataframes to evaluate the Ratio of Counts per variable 
  ### num_cat is the number of categories that can be evaluated 
  ### min_items is the minimum number of items in a category, otherwise it will be omitted from calculations
  ROC = 0 
  num_col = 0 ## counter for the number of columns to divide ROC by 
  for (x in colnames(data1)){
    df_freq <- tabyl(data1, (x))
    if (nrow(df_freq) <= num_cat){
    df_freq[[x]] <- as.character(df_freq[[x]])
    df_freq[[x]][is.na(df_freq[[x]])] = 0 ## Replaced NA values with 0 - have to ensure that there arent any 0 values in real dataset
    df_freq <- data.frame(df_freq, row.names = df_freq[[x]])
    dfsyn_freq <- tabyl(data2, (x))
    dfsyn_freq[[x]] <- as.character(dfsyn_freq[[x]])
    dfsyn_freq[[x]][is.na(dfsyn_freq[[x]])] = 0 
    dfsyn_freq <- data.frame(dfsyn_freq,row.names = dfsyn_freq[[x]])
    
    
    df_combined <- merge(df_freq, dfsyn_freq, by = 0, all = TRUE) ## merge by row name which is every single individual value of a row 
    if(nrow(df_combined) <= num_cat){
      df_combined$min <- pmin(df_combined[c('percent.x')], df_combined[c('percent.y')])
      df_combined$max <- pmax(df_combined[c('percent.x')], df_combined[c('percent.y')])
      df_combined$ratio <- df_combined$min / df_combined$max
      df_combined$ratio[is.na(df_combined$ratio)] <- 0
      w <- which(df_combined$n.x ==0 & df_combined$n.y ==0)
      if (is.integer0(w) == FALSE){df_combined <- df_combined[-c(w),]}
      
      y <- which(df_combined$ratio == 0 & (df_combined$n.x <= min_items & df_combined$n.y <= min_items))
      if (is.integer0(y) == FALSE){next} 
      
      num_col = num_col + 1 
    ROC = ROC + as.numeric(lapply(df_combined[,ncol(df_combined)], mean))}}}
  
  ROC/num_col  
  
}

ROC_indiv <- function (data1, data2, x,  num_cat = 2000){
  ### Read in 2 dataframes to evaluate the Ratio of Counts per variable per column 
  ### x is the column name that we want to compare ROC on 
  ### num_cat is the number of categories that can be evaluated 

  df_freq <- tabyl(data1, (x))
  if (nrow(df_freq) <= num_cat){
    df_freq[[x]] <- as.character(df_freq[[x]])
    df_freq[[x]][is.na(df_freq[[x]])] = 0 ## Replaced NA values with 0 - have to ensure that there arent any 0 values in real dataset
    df_freq <- data.frame(df_freq, row.names = df_freq[[x]])
    dfsyn_freq <- tabyl(data2, (x))
    dfsyn_freq[[x]] <- as.character(dfsyn_freq[[x]])
    dfsyn_freq[[x]][is.na(dfsyn_freq[[x]])] = 0 
    dfsyn_freq <- data.frame(dfsyn_freq,row.names = dfsyn_freq[[x]])
    
    
    df_combined <- merge(df_freq, dfsyn_freq, by = 0, all = TRUE) ## merge by row name which is every single individual value of a row 
    if(nrow(df_combined) <= num_cat){
      df_combined$min <- pmin(df_combined[c('percent.x')], df_combined[c('percent.y')])
      df_combined$max <- pmax(df_combined[c('percent.x')], df_combined[c('percent.y')])
      df_combined$ratio <- df_combined$min / df_combined$max
      df_combined$ratio[is.na(df_combined$ratio)] <- 0
      w <- which(df_combined$n.x ==0 & df_combined$n.y ==0)
      if (is.integer0(w) == FALSE){df_combined <- df_combined[-c(w),]}
      
      ROC = as.numeric(lapply(df_combined[,ncol(df_combined)], mean))}
      print(paste0(x, " : ",  ROC))}
  else {print (paste0(x, " : not used in calculation as number of categories exceed num_cat."))}
 }

is.integer0 <- function(x){
  is.integer(x) && length(x) ==0L
}


ROC_numeric <- function(data1, data2, x, y = 2500 , num_cat=2000){
  df_freq <- tabyl(HDB_complete, (x))
  df_freq[[x]] <- round_any(as.numeric(df_freq[[x]]), y)
  df_freq <- data.frame(aggregate(cbind(df_freq$n, df_freq$percent), by = list(df_freq[[x]]), FUN = sum))
  colnames(df_freq) <- c(paste(x), "n", "percent")

  dfsyn_freq <- tabyl(HDB_syn1$syn, (x))
  dfsyn_freq[[x]] <- round_any(as.numeric(dfsyn_freq[[x]]), y) 
  dfsyn_freq <- data.frame(aggregate(cbind(dfsyn_freq$n, dfsyn_freq$percent), by = list(dfsyn_freq[[x]]), FUN = sum))
  colnames(dfsyn_freq) <- c(paste(x), "n", "percent")

  if (nrow(df_freq) <= num_cat){
    df_combined <- merge(df_freq, dfsyn_freq, by = "GrantAmount", all = TRUE) ## merge by row name which is every single individual value of a row 
    if(nrow(df_combined) <= num_cat){
      df_combined$min <- pmin(df_combined[c('percent.x')], df_combined[c('percent.y')])
      df_combined$max <- pmax(df_combined[c('percent.x')], df_combined[c('percent.y')])
      df_combined$ratio <- df_combined$min / df_combined$max
      df_combined$ratio[is.na(df_combined$ratio)] <- 0
      w <- which(df_combined$n.x ==0 & df_combined$n.y ==0)
      if (is.integer0(w) == FALSE){df_combined <- df_combined[-c(w),]}
    
  ROC = as.numeric(lapply(df_combined[,ncol(df_combined)], mean))}}
  print(ROC) }

ROC_list <- function(data1, data2, num_cat = 10000){
  for (x in colnames(data1)){
    ROC_indiv(data1, data2, x)
    }
}


