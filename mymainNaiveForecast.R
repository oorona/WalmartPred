mypredict = function(){
  
indexcols=c("Store","Dept")
indexcolsdate=c("Store","Dept","Date")

if (t > 1){
  train <<- train %>% add_row(new_train)
}

#Calculate Dates
#initial_date=ymd("2010-02-05")
sstart_date="2011-03-01"
send_date="2011-05-01"

start_date=ymd(sstart_date) %m+% months(2 * (t - 1))
end_date = ymd(send_date) %m+% months(2 * (t - 1))
#nstart_date <-ymd(sstart_date)  %m+% months(2 * (t - 1)-12)
#nend_date <- ymd(send_date) %m+% months(2 * (t - 1) -12)


#num_of_weeks=trunc(as.double(difftime(start_date, initial_date,units = 'weeks')))
#num_of_pred_weeks=trunc(as.double(difftime(end_date, ymd(sstart_date),units = 'weeks')))
#endw=56+num_of_pred_weeks


# find data for testing between dates
test_split = test %>% 
  filter(Date >= start_date & Date < end_date) %>% 
  select(-IsHoliday) 


# find unique combinatios of stre and dept in both test and training
train_store_dept=train[, indexcols] %>% 
  count(Store, Dept)

test_store_dept=test_split[, indexcols] %>% 
  count(Store, Dept) 

common_store_dept=intersect(train_store_dept[, indexcols], test_store_dept[, indexcols])

#Filter only relevant data 
train_split= common_store_dept %>% 
  left_join(train, by = indexcols) %>% 
  select(-IsHoliday) 

# Find validate Dates
pred_dates=test_split %>% 
  distinct(Date)

report_dates=train_split %>% 
  distinct(Date)

# Create full set of date store and Dept
full_report_dates=report_dates %>% 
  full_join(common_store_dept, by=character())

full_pred_dates=pred_dates %>% 
  full_join(common_store_dept, by=character())

#Expand data to valid dates
train_store_dept_fix=full_report_dates %>% 
  left_join(train_split,by=indexcolsdate) %>% 
  replace_na(list(Weekly_Sales=0)) 

test_store_dept_fix=full_pred_dates %>% 
  left_join(test_split,by=indexcolsdate) %>% 
  mutate(Weekly_Pred=0)

#Transform data for faster processing

train_data=train_store_dept_fix %>%  
  pivot_wider(names_from = c(Store,Dept),values_from=Weekly_Sales)

test_data=test_store_dept_fix %>%  
  pivot_wider(names_from = c(Store,Dept),values_from=Weekly_Pred)

num_forecasts=dim(test_data)[1]

#pred_list = vector(mode = "list", length = nrow(common_store_dept))

for (i in 1:nrow(common_store_dept)) {
    #Seasonal Naive
    predn=train_data[seq(nrow(train_data)-51,nrow(train_data)-51+num_forecasts-1),i+1]  
    
    #Tslm
    data_ts = ts(train_data[,i+1], frequency=52) 
    data_fit = tslm(data_ts ~ season + trend) 
    predt = tibble(pred=forecast(data_fit, h = num_forecasts)$mean)
    
    predt =predt %>% 
      mutate(pred=ifelse(pred<0,0,pred))
    pred=0.25*predn+0.75*predt
    test_data[,i+1]= pred    
}

#Transform data back to normal
test_final=test_data %>%  
  pivot_longer(!Date,names_to = c("Store","Dept"),values_to="Weekly_Pred",names_sep="_")

test_final= test_final %>% 
  mutate(Store=as.integer(Store)) %>% 
  mutate(Dept=as.integer(Dept))

# Select only data required for prediction
test_pred=test_split %>% 
  inner_join(test_final, by=indexcolsdate)

return(test_pred)
}

