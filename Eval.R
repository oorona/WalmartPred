#! /usr/bin/Rscript
suppressPackageStartupMessages(library("dplyr")) 
suppressPackageStartupMessages(library("lubridate")) 
suppressPackageStartupMessages(library("tidyr")) 
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  num_folds = 10
  script="mymain.R"
} else  {
  script = args[1]
  num_folds =args[2]
}

library (tidyr)
library(lubridate)
library(dplyr)
library(readr)
library(forecast)


source(script)

# read in train / test dataframes
train <- readr::read_csv('train_ini.csv')
test <- readr::read_csv('test.csv')

# save weighted mean absolute error WMAE

wae <- rep(0, num_folds)

for (t in 1:num_folds) {
  # *** THIS IS YOUR PREDICTION FUNCTION ***
  tm=system.time(test_pred <- mypredict())
  print(tm)
  #print(dim(train))
  #print(dim(test_pred))

  #print(head(test_pred,10))
  
  # load fold file 
  fold_file <- paste0('fold_', t, '.csv')
  new_train <- readr::read_csv(fold_file, 
                               col_types = cols())
  
  # extract predictions matching up to the current fold
  scoring_tbl <- new_train %>% 
    left_join(test_pred, by = c('Date', 'Store', 'Dept'))
  
  # compute WMAE
  actuals <- scoring_tbl$Weekly_Sales
  preds <- scoring_tbl$Weekly_Pred
  preds[is.na(preds)] <- 0
  weights <- if_else(scoring_tbl$IsHoliday, 5, 1)
  wae[t] <- sum(weights * abs(actuals - preds)) / sum(weights)
  
    start_date <- ymd("2010-03-01") %m+% months(2 * (t - 1))
    end_date <- ymd("2010-05-01") %m+% months(2 * (t - 1))
    cat("-------------------",t,"-----------------------")
    print(scoring_tbl %>% 
            filter(Store==1,Dept==18) %>% 
            mutate(CDate=Date-weeks(52)) %>% 
            select(Date,CDate,Weekly_Sales,Weekly_Pred))
    print(train %>% filter(Store==1,Dept==18) %>% 
            filter(Date >= start_date & Date < end_date) %>% 
            select(Store,Dept,Date,Weekly_Sales))
  
}

print(wae)
mean(wae)
