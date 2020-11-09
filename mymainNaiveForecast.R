mypredict = function(){
  
indexcols=c("Store","Dept")

if (t > 1){
  train <<- train %>% add_row(new_train)
}

#Calculate Dates
initial_date=ymd("2010-02-05")
start_date=ymd("2011-03-01") %m+% months(2 * (t - 1))
end_date = ymd("2011-05-01") %m+% months(2 * (t - 1))

num_of_weeks=trunc(as.double(difftime(start_date, initial_date,units = 'weeks')))
num_of_pred_weeks=trunc(as.double(difftime(end_date, ymd("2011-03-01"),units = 'weeks')))
endw=56+num_of_pred_weeks
report_dates=tibble(Date=initial_date+weeks(0:num_of_weeks))
pred_dates=tibble(Date=initial_date+weeks(56:endw))

pred_dates=pred_dates%>% 
  filter(Date >= start_date & Date < end_date)

#print(pred_dates)
# find data for testing between dates
test_split = test %>% 
  filter(Date >= start_date & Date < end_date) %>% 
  select(-IsHoliday) 


nstart_date <- ymd("2010-03-01") %m+% months(2 * (t - 1))
nend_date <- ymd("2010-05-01") %m+% months(2 * (t - 1))
#print("-----------------------------first")
#print(train %>% filter(Store==1,Dept==18) %>% 
#        filter(Date >= nstart_date & Date < nend_date) %>% 
#        select(Store,Dept,Date,Weekly_Sales))

train_store_dept=train[, indexcols] %>% 
  count(Store, Dept)

test_store_dept=test_split[, indexcols] %>% 
  count(Store, Dept) 

common_store_dept=intersect(train_store_dept[, indexcols], test_store_dept[, indexcols])

#Filter only train data  for important state and count

train_split= common_store_dept %>% 
  left_join(train, by = indexcols) %>% 
  #mutate(Date=as_date(ifelse(year(Date) == 2010, Date-7, Date)))%>% 
  select(-IsHoliday) 

# train_split %>% 
#   filter(Store==2, Dept==1)
# 
# test_split %>% 
#   filter(Store==1, Dept==1)
# 
# test_split

pred_list = vector(mode = "list", length = nrow(common_store_dept))
for (i in 1:nrow(common_store_dept)) {
  #if (i<2) {
    train_store_dept= train_split %>% 
      filter(Store==common_store_dept[i,]$Store, Dept==common_store_dept[i,]$Dept) %>% 
      select(Date,Weekly_Sales)
    test_store_dept= test_split %>% 
      filter(Store==common_store_dept[i,]$Store, Dept==common_store_dept[i,]$Dept) %>% 
      select(Store,Dept,Date)
    
    train_store_dept_fix=report_dates %>% 
      left_join(train_store_dept,by=c("Date")) %>% 
      replace_na(list(Weekly_Sales=0)) 
    test_store_dept_fix=pred_dates %>% 
      left_join(test_store_dept,by=c("Date")) 
    
    # if (i==17) {
    #   nstart_date <- ymd("2010-03-01") %m+% months(2 * (t - 1))
    #   nend_date <- ymd("2010-05-01") %m+% months(2 * (t - 1))
    #   print("train_store_dept_fix")
    #   
    #   print(train_store_dept_fix %>%  
    #                   filter(Date >= nstart_date & Date < nend_date) )
    #   print("test_store_dept")
    #   print(test_store_dept)
    #   print("test_store_dept_fix")
    #   print(test_store_dept_fix )
    # }
    
    
    #cat("Store",common_store_dept[i,]$Store,"Dept",common_store_dept[i,]$Dept,"\n")
    num_forecasts=dim(test_store_dept_fix)[1]
    #print(num_forecasts)
    data_ts = ts(train_store_dept_fix[,2], frequency=52) 
    predn = snaive(data_ts, num_forecasts)$mean 
    data_fit = tslm(data_ts ~ season + trend) 
    predt = forecast(data_fit, h = num_forecasts)$mean
    
    pred=(predn+predt)/2
    pred=cbind(test_store_dept_fix, Weekly_Pred = pred) 
    sel_pred = test_store_dept %>% 
      left_join(pred, by=c("Store","Dept","Date")) %>% 
      mutate(Weekly_Pred=ifelse(Weekly_Pred<0,0,Weekly_Pred))
    # if (i==17) {
    #   print("------------Pred")
    #   print(sel_pred)
    #   #print(sel_pred)
    # }
    pred_list[[i]] = sel_pred
    #pred_list[[i]] <- cbind(test_store_dept, Weekly_Pred = pred)
    #print(pred_list[[i]])
    #T = dim(train_store_dept)[1]
    #print(T)
    #print(train_store_dept[[T,1]] )
    #print(pred)
  #}
    #if(i==17){
    #  cat("Store",common_store_dept[i,]$Store,"Dept",common_store_dept[i,]$Dept,"\n")
    #  print("test_store_dept")
    #  print(test_store_dept)
    #  print("train_store_dept")
    #  print(train_store_dept_fix[15:25,])
      #print(train_split %>% 
      #        filter(Store==common_store_dept[i,]$Store, Dept==common_store_dept[i,]$Dept))
    #}
}
final_pred <- bind_rows(pred_list)
return(final_pred)
}

