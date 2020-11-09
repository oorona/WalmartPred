mypredict = function() {
  start_date <- ymd("2011-03-01") %m+% months(2 * (t - 1))
  end_date <- ymd("2011-05-01") %m+% months(2 * (t - 1))
  test_current <- test %>%
    filter(Date >= start_date & Date < end_date) %>%
    select(-IsHoliday)
  
  if (t>1){
    train <<- train %>% add_row(new_train)
  }
  
  start_last_year = min(test_current$Date) - 375
  end_last_year = max(test_current$Date) - 350
  tmp_train <- train %>%
    filter(Date > start_last_year & Date < end_last_year) %>%
    mutate(Wk = ifelse(year(Date) == 2010, week(Date)-1, week(Date))) %>%
    rename(Weekly_Pred = Weekly_Sales) %>%
    select(-Date, -IsHoliday)
  
  test_current <- test_current %>%
    mutate(Wk = week(Date))
  
  test_pred <- test_current %>%
    left_join(tmp_train, by = c('Dept', 'Store', 'Wk')) %>%
    select(-Wk)
  return(test_pred)
}