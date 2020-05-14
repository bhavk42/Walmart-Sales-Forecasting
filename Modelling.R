# loading libraries and packages
install.packages('onehot')
install.packages('itsmr')
install.packages('Metrics')
detach("package:itsmr",unload = TRUE)
library(itsmr)
library(ggplot2)
library(onehot)
library(data.table)
library(reshape)
library(dplyr)
library('ggplot2')
library('forecast')
library('tseries')
library(tidyr)
library(Metrics)
# reading the dataframe
df = read.csv('D:/down/m5-forecasting-uncertainty/sales_train_validation.csv')
dim(df)[2]
names(df)
# reshaping the dataframe
pivot_longer(df,cols = -c("id","item_id","dept_id","cat_id","store_id","state_id"),names_to = 'day')
df_pivot = pivot_longer(df,cols = -c("id","item_id","dept_id","cat_id","store_id","state_id"),names_to = 'day')
head(df_pivot)
# reading the calendar file
calendar = read.csv('D:/down/m5-forecasting-uncertainty/calendar.csv')
head(calendar)
calendar = calendar[,c('wm_yr_wk','d')]
sell_prices = read.csv('D:/down/m5-forecasting-uncertainty/sell_prices.csv')
head(sell_prices)
# joining the datasets
sell_prices = inner_join(sell_prices,calendar,by='wm_yr_wk')
names(sell_prices)[5] <- 'day'
names(sell_prices)
sell_prices$day =as.character(sell_prices$day)
df_final = left_join(df_pivot,sell_prices,on=c('store_id','item_id','day'))
dim(df_pivot)
dim(df_final)
df_final[is.na(df_final$sell_price)]
df_final %>% filter_all(any_vars(is.na(.))) %>% filter(value != 0) %>% select(value,sell_price)
df_final <- na.omit(df_final)
dim(df_final)
names(df_final)
df_final$Sales = df_final$value*df_final$sell_price
# aggregate sales value for items
df_Sales = df_final %>% group_by(day) %>% summarise('Sales' = sum(Sales))
df_Sales_state = df_final %>% group_by(state_id,day) %>% summarise('Sales' = sum(Sales))
write.csv(df_Sales_state,'D:/down/df_sales_state.csv')
dim(df_Sales)
df_Sales
get_day =  function(string){
  day =as.integer(strsplit(string,'_')[[1]][2])
  return (day)
}

df_Sales$day = sapply(df_Sales$day,get_day)
names(df_Sales)
df_Sales = df_Sales[order(df_Sales$day),]
plot(ts(df_Sales$Sales))
# writing the file
write.csv(df_Sales,'D:/down/df_sales.csv')
head(df_Sales)
head(calendar)
calendar$d = as.character(calendar$d)
calendar$d
calendar$day =  sapply(calendar$d,get_day)
names(calendar)
total_dataset =merge(df_Sales,calendar,by.x = 'day',by.y = 'day')
total_dataset$date = as.Date(as.character(total_dataset$date))
head(total_dataset)
min(total_dataset$date)
max(total_dataset$date)
# filter out to complete  periods
total_dataset_monthly= total_dataset[total_dataset$date >= as.Date('2011-02-01') & total_dataset$date < as.Date('2016-04-01'), ]
min(total_dataset_monthly$date)
max(total_dataset_monthly$date)
max(total_dataset$wm_yr_wk)
total_dataset_weekly =total_dataset[total_dataset$wm_yr_wk !=11613,]
total_dataset_weekly_train = total_dataset_weekly[total_dataset_weekly$date < as.Date('2015-01-03'),]
total_dataset_weekly_test = total_dataset_weekly[total_dataset_weekly$date >= as.Date('2015-01-03'),]
total_dataset_monthly_train = total_dataset_monthly[total_dataset_monthly$date < as.Date('2015-01-01'),]
total_dataset_monthly_test = total_dataset_monthly[total_dataset_monthly$date >= as.Date('2015-01-01'),]
dim(total_dataset_monthly_test)
# Summarise weekly and monthly
Weekly_Dataset_test = total_dataset_weekly_test %>% group_by(wm_yr_wk) %>% summarise(Sales= sum(Sales))
weekly_Dataset_test = ts(Weekly_Dataset_test$Sales)
Weekly_Dataset_train = total_dataset_weekly_train %>% group_by(wm_yr_wk) %>% summarise(Sales= sum(Sales))
weekly_Dataset_train = ts(Weekly_Dataset_train$Sales)
# plotting ACf and pacf for weekly dataset
plot(weekly_Dataset_train)
acf(weekly_Dataset_train)
pacf(weekly_Dataset_train)
Monthly_Dataset_test = total_dataset_monthly_test%>% group_by(year,month) %>% summarise(Sales= sum(Sales))
monthly_data_test = ts(Monthly_Dataset_test$Sales)
length(monthly_data_test)
Monthly_Dataset_train = total_dataset_monthly_train%>% group_by(year,month) %>% summarise(Sales= sum(Sales))
monthly_data_train = ts(Monthly_Dataset_train$Sales)
plot(monthly_data_train/10000,xlab='Months',ylab='Sales in 10,000$',main='Plot of Monthly Data',col='red',las=1,)
acf(monthly_data_train,main= 'ACF plot of Monthly Dataset')
pacf(monthly_data_train,main='PACF plot of Monthly Dataset')
diff1 = diff(monthly_data_train,differences = 1)
plot(ts(diff1))
acf(ts(diff1),lag.max = 40,main='ACF after differencing at lag 1')
pacf(ts(diff1),lag.max = 40,main='PACF after differencing at lag 1')
adf.test(diff1,alternative = 'stationary')
kpss.test(diff1,null = 'Level')
test(diff1)
diff3 = diff(monthly_data_train,lag = 12)
diff3 = diff(monthly_data_train,differences = 1,lag = 1)
acf(ts(diff3),lag.max = 40,main='ACF after differencing at lag 1 and 12')
pacf(ts(diff3),lag.max = 40,main='PACF after differencing at lag 1 and 12')
adf.test(diff3,alternative = 'stationary')
kpss.test(diff3,null = 'Level')
####
auto_result = Arima(monthly_data_train)
summary(auto_result)
####
monthly_seasonal= Arima(monthly_data_train,order=c(1,1,1),include.drift = TRUE,seasonal = list(order=c(1,1,1),period=12))
summary(monthly_seasonal)
residuals()
plot(monthly_seasonal$residuals)
plot(ts_res1)
qqnorm(ts_res1, main = "", cex.axis = 1.2, cex.lab = 1.45)
qqline(ts_res1, col = "red")
####
monthly =Arima(monthly_data_train,order = c(1,1,1),include.drift = TRUE)
summary(monthly)
plot(monthly$residuals)
#####
plot(forecast(monthly_seasonal,h=5))
plot(forecast(monthly,h=5))
fit_seasonal_pred_monthly = forecast::forecast(monthly_seasonal,h=15)
fit_seasonal_pred_monthly = fit_seasonal_pred_monthly$mean
forecast::accuracy(object=as.numeric(fit_seasonal_pred_monthly),x= as.numeric(monthly_data_test))
plot(ts(as.numeric(fit_seasonal_pred_monthly)-as.numeric(monthly_data_test)))
plot(monthly)
####
plot(forecast::forecast(monthly,h=15))
fit_monthly_pred = forecast(monthly,h=15)
fit_monthly_pred = fit_monthly_pred$mean
forecast::accuracy(object=as.numeric(fit_monthly_pred),x= as.numeric(monthly_data_test))
plot(monthly_seasonal)
plot(predict(forecast::forecast(monthly_seasonal,h=15)),xlab = 'Months',ylab='Sales$') 
line(as.numeric(monthly_data_test))
ts_res2 = monthly$residuals
qqnorm(ts_res2, main = "", cex.axis = 1.2, cex.lab = 1.45)
qqline(ts_res2, col = "red")
ggplo
adf.test(monthly_data_train)
####
weekly_Data=ts(weekly_Dataset_train)
plot(weekly_Data)
plot(weekly_Data/10000,xlab='Weeks',ylab='Sales in 10,000$',main='Plot of Weekly Data',col='red',las=1,)
acf(weekly_Data,main= 'ACF plot of Weekly Dataset')
pacf(weekly_Data,main='PACF plot of Weekly Dataset')
tabts_model =tbats(weekly_Data,seasonal.periods = c(4.34,52.179),use.box.cox = TRUE)
tabts_model
predict_weekly = forecast(tabats_model,h=68)
predict_weekly = predict_weekly$mean
forecast::accuracy(object = as.numeric(predict_weekly),x=weekly_Dataset_test)
fit_weekly =forecast(tabts_model,h=15)
weekly_Data %>% mstl() %>%
  autoplot() + xlab("Week")
diff1_weekly = diff(weekly_Data,diferences =1)
adf.test(diff3_weekly,alternative = 'stationary')
kpss.test(diff3_weekly,null = 'Trend')
plot(ts(diff1_weekly))
acf(ts(diff1_weekly))
pacf(ts(diff1_weekly))
diff2_weekly = diff(diff1_weekly,lag=4)
plot(diff2_weekly)
acf(diff2_weekly,lag.max =50)
pacf(diff2_weekly,lag.max=50)
diff3_weekly = diff(weekly_Data,lag=52,differences = 2)
diff3_weekly = diff(diff3_weekly,lag = 1,differences = 2)
plot(ts(diff3_weekly))
acf(ts(diff3_weekly),lag.max = 60,main='ACF after differencing at lags 1,52 twice')
pacf(ts(diff3_weekly),lag.max = 60,main='PACF after differencing at lags 1,52 twice')
weekly_fit = Arima(weekly_Data,order=c(5,2,1),include.drift = TRUE,seasonal =list(order=c(0,2,1),period=52))
summary(weekly_fit)
auto.arima(weekly_Data)
plot(weekly_fit)
plot(forecast::forecast(weekly_fit,h=68))
ts_res3 = weekly_fit$residuals
plot(ts_res3)
qqnorm(ts_res3, main = "", cex.axis = 1.2, cex.lab = 1.45)
qqline(ts_res3, col = "red")
     

predict_weekly_arima = forecast::forecast(weekly_fit,h=68)
predict_weekly_arima = predict_weekly_arima$mean
forecast::accuracy(object = as.numeric(predict_weekly_arima),x=weekly_Dataset_test)

# multivariate
df_Sales_multi = df_final %>% group_by(cat_id,state_id,day) %>% summarise(Sales =sum(Sales))
df_Sales_multi$day = sapply(df_Sales_multi$day,get_day)
total_dataset_multi =merge(df_Sales_multi,calendar,by.x = 'day',by.y = 'day')
Monthly_Dataset_multi =as.data.frame(total_dataset_multi%>% group_by(year,month,cat_id,state_id) %>% summarise(Sales= sum(Sales)))
Monthly_Dataset_multi
test =onehot(Monthly_Dataset_multi,stringsAsFactors =TRUE)
as.data.frame(test)
Monthly_Dataset_multi =as.data.frame(predict(test,Monthly_Dataset_multi))
head(Monthly_Dataset_multi) 

