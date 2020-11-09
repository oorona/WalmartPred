library (tidyr)
library(lubridate)
library(dplyr)
library(forecast)
library()
train <- readr::read_csv('train_ini.csv')
test <- readr::read_csv('test.csv')


t=1

start_date <- ymd("2011-03-01") %m+% months(2 * (t - 1))

end_date <- ymd("2011-05-01") %m+% months(2 * (t - 1))



test_current <- test %>%
  filter(Date >= start_date & Date < end_date) %>%
  select(-IsHoliday)

most_recent_date <- max(train$Date)

tmp_train <- train %>%
  filter(Date == most_recent_date) %>%
  rename(Weekly_Pred = Weekly_Sales) %>%
  select(-Date, -IsHoliday)

test_pred <- test_current %>%
  left_join(tmp_train, by = c('Dept', 'Store')) 

train %>% 
  group_by(Store, Dept) %>% 
  summarize(mean_sales=mean(Weekly_Sales,na.rm=TRUE)) %>% 
  print(50)

train %>% 
  count(Store, Dept) %>% 
  arrange(desc(Dept))

          

all_depts = unique(train$Dept) 
dept = all_depts[5]
all_depts

train_dept_ts=train %>% 
  filter(Date < start_date) %>% 
  filter(Dept == dept) %>% 
  select(Store, Date, Weekly_Sales) %>% 
  spread(Store, Weekly_Sales) 


train_dept_ts

dim(train_dept_ts)
range(train_dept_ts$Date)

diff(range(train_dept_ts$Date))

train_dept_ts

j=3


train_dept_ts[,j]

store_ts = ts(train_dept_ts[, j], frequency=52) 

T = length(store_ts)
  
num_forecasts = 7


naive.pred=naive(store_ts, num_forecasts)$mean
store_ts[T]
naive.pred


snaive.pred = snaive(store_ts, num_forecasts)$mean 
last.year.index = (T-52)+(1:num_forecasts)

store_ts[last.year.index] 
snaive.pred


myfit = tslm(store_ts ~ season + trend) 

str(forecast(myfit, h = num_forecasts))

tslm.pred = forecast(myfit, h = num_forecasts)$mean
tslm.pred


tmpdata = data.frame(   
  Y = as.numeric(store_ts),    
  season = as.factor((1:T) %% 52),   
  trend = trunc((1:T)/52)   
  ) 

tmpdata

myfit = lm(Y ~ season + trend, tmpdata)


testindex = (T+1):(T+num_forecasts) 

testdata = data.frame(   
  season = as.factor(testindex %% 52),    
  trend = trunc(testindex/52) 
  )

predict(myfit, newdata = testdata) 
tslm.pred

train

test_pred

train %>% 
  filter(Date < start_date) %>% 
  select(Store, Dept,Date,Weekly_Sales) %>% 
    pivot_wider(values(), Weekly_Sales) %>% 
head

dim(train_dept_ts)


combs=train %>%
  distinct(across(c(Store,Dept)))

num_preds=dim(combs)[0]

tmp_train = train %>% 
  filter(Date < start_date) %>% 
  select(Store, Dept,Date,Weekly_Sales) %>% 
  pivot_wider(names_from = c(Store,Dept),values_from=Weekly_Sales)
  
test_current <- test %>%
  filter(Date >= start_date & Date < end_date) %>%
  select(-IsHoliday)

tmp_test = test_current %>% 
  select(Store, Dept,Date) %>%
  mutate(Weekly_Pred=0) %>% 
  pivot_wider(names_from = c(Store,Dept),values_from=Weekly_Pred)
  
dim(tmp_test)

head(tmp_test,20)

tmp_test[,1]=c[[1,2,3,4,5,6,7,8,9]]

col_names=colnames(tmp_train)

col_names
num_forecasts=7

for (p in c(2:num_preds+1)) {
  print(col_names[p])  
  data_ts=ts(tmp_train[,p],frequency=52)
  pred=naive(data_ts,num_forecasts)
  print(pred)
}



