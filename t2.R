library (tidyr)
library(lubridate)
library(dplyr)
library(forecast)
train <- readr::read_csv('train_ini.csv')
test <- readr::read_csv('test.csv')

t=1

indexcols=c("Store","Dept")
indexcolsdate=c("Store","Dept","Date")

if (t > 1){
  train <<- train %>% add_row(new_train)
}

#Calculate Dates
initial_date=ymd("2010-02-05")
sstart_date="2011-03-01"
send_date="2011-05-01"

start_date=ymd(sstart_date) %m+% months(2 * (t - 1))
end_date = ymd(send_date) %m+% months(2 * (t - 1))
nstart_date <-ymd(sstart_date)  %m+% months(2 * (t - 1)-12)
nend_date <- ymd(send_date) %m+% months(2 * (t - 1) -12)


num_of_weeks=trunc(as.double(difftime(start_date, initial_date,units = 'weeks')))
num_of_pred_weeks=trunc(as.double(difftime(end_date, ymd(sstart_date),units = 'weeks')))
endw=56+num_of_pred_weeks
#report_dates=tibble(Date=initial_date+weeks(0:num_of_weeks))
#pred_dates=tibble(Date=initial_date+weeks(56:endw))

#pred_dates=pred_dates%>% 
#  filter(Date >= start_date & Date < end_date)

# find data for testing between dates
test_split = test %>% 
  filter(Date >= start_date & Date < end_date) %>% 
  select(-IsHoliday) 


train_store_dept=train[, indexcols] %>% 
  count(Store, Dept)

test_store_dept=test_split[, indexcols] %>% 
  count(Store, Dept) 

common_store_dept=intersect(train_store_dept[, indexcols], test_store_dept[, indexcols])

#Filter only train data  for important state and count

train_split= common_store_dept %>% 
  left_join(train, by = indexcols) %>% 
  select(-IsHoliday) 

pred_dates=test_split %>% 
  distinct(Date)

report_dates=train_split %>% 
  distinct(Date)

full_report_dates=report_dates %>% 
  full_join(common_store_dept, by=character())

full_pred_dates=pred_dates %>% 
  full_join(common_store_dept, by=character())

train_store_dept_fix=full_report_dates %>% 
  left_join(train_split,by=indexcolsdate) %>% 
  replace_na(list(Weekly_Sales=0)) 

test_store_dept_fix=full_pred_dates %>% 
  left_join(test_split,by=indexcolsdate) %>% 
  mutate(Weekly_Pred=0)


train_data=train_store_dept_fix %>%  
  pivot_wider(names_from = c(Store,Dept),values_from=Weekly_Sales)

test_data=test_store_dept_fix %>%  
  pivot_wider(names_from = c(Store,Dept),values_from=Weekly_Pred)

num_forecasts=dim(test_data)[1]


pred_list = vector(mode = "list", length = nrow(common_store_dept))
#print(dim(test_data))
#cat("number of weeks",num_of_weeks, "End week",endw,"diff",endw-num_of_weeks)
#print(pred_dates)
for (i in 1:nrow(common_store_dept)) {
  # train_store_dept= train_store_dept_fix %>% 
  #   filter(Store==common_store_dept[i,]$Store, Dept==common_store_dept[i,]$Dept) %>% 
  #   select(Date,Weekly_Sales)
  # test_store_dept= test_store_dept_fix %>% 
  #   filter(Store==common_store_dept[i,]$Store, Dept==common_store_dept[i,]$Dept) %>% 
  #   select(Store,Dept,Date)
  
  #num_forecasts=dim(test_store_dept)[1]
  predn=train_data[seq(nrow(train_data)-51,nrow(train_data)-51+num_forecasts-1),i+1]  
  data_ts = ts(train_data[,i+1], frequency=52) 
  #predn = snaive(data_ts, num_forecasts)$mean 
  data_fit = tslm(data_ts ~ season + trend) 
  predt = tibble(forecast(data_fit, h = num_forecasts)$mean)
  pred=(predn+predt)/2
  #pred=predn
  #pred=predt
  test_data[,i+1]= pred    
  # if (i==st-1 && t==3){
  #   cat("Start_date",sstart_date,"End_data",send_date)
  #   print(num_of_weeks)
  #   print(pred_dates)
  #   print(nrow(train_data))
  #   print(seq(nrow(train_data)-51,nrow(train_data)-51+num_forecasts-1))
  #   print("-------------data-----------")
  #   print(train_data[seq(nrow(train_data)-51,nrow(train_data)-51+num_forecasts-1),c(1,i+1)])
  #   print("-------------pred-----------")
  #   print(predn)
  #   #print (pred)
  #   print(test_data[,c(1,i+1)])
  # }
  
  # pred=cbind(test_store_dept, Weekly_Pred = pred) 
  # sel_pred = test_store_dept %>% 
  #   left_join(pred, by=indexcolsdate) %>% 
  #   mutate(Weekly_Pred=ifelse(Weekly_Pred<0,0,Weekly_Pred))
  # 
  # pred_list[[i]] = sel_pred
}
#final_pred <- bind_rows(pred_list)

predt
predn

test_final=test_data %>%  
  pivot_longer(!Date,names_to = c("Store","Dept"),values_to="Weekly_Pred",names_sep="_")

test_final= test_final %>% 
  mutate(Store=as.integer(Store)) %>% 
  mutate(Dept=as.integer(Dept))

test_pred=test_split %>% 
  inner_join(test_final, by=indexcolsdate)

?seq
