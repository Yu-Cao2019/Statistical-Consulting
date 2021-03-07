# load packages
library(forecast)
library(tseries)
library(laeken)
library(xts)
library(zoo)
library(trend)

# load data
project1<-read.csv(file="daily_data_08-20.csv")

# pre-processing
which(is.na(project1))

which(project1['PM25']<0)

project1['PM25'][project1['PM25'] < 0] = NA

project1['date'][is.na(project1['PM25'])]

project10 <- project1[-c(1:633),]
naindex10=is.na(project10[,2])
project12 <- project1[-c(1:1363),]
naindex12=is.na(project12[,2])

# missing value
meanvalue=mean(project10[complete.cases(project10),'PM25'])
project10[naindex10,2]=meanvalue
which(is.na(project10))
meanvalue12=mean(project12[complete.cases(project12),'PM25'])
project12[naindex12,2]=meanvalue12
which(is.na(project12))

write.csv(project10,file="/Users/apple/Documents/me/GWU/courses/6245/Project1/final/project10.csv",quote=F,row.names = F)
write.csv(project12,file="/Users/apple/Documents/me/GWU/courses/6245/Project1/final/project12.csv",quote=F,row.names = F)

# monthly
t10 <- as.Date(project10$date, format='%m/%d/%y')
pm10 <- data.frame(PM25=project10$PM25)
pro10 <- zoo(pm10, t10)
month_project10 <- pro10[endpoints(pro10, on='months', k=1),]

t12 <- as.Date(project12$date, format='%m/%d/%y')
pm12 <- data.frame(PM25=project12$PM25)
pro12 <- zoo(pm12, t12)
month_project12 <- pro12[endpoints(pro12, on='months', k=1),]

write.csv(month_project10,file="/Users/apple/Documents/me/GWU/courses/6245/Project1/final/month_project10.csv",quote=F)
write.csv(month_project12,file="/Users/apple/Documents/me/GWU/courses/6245/Project1/final/month_project12.csv",quote=F)

# 10-20 daily data
PM25_d_10<-ts(project10['PM25'],frequency = 365,start=c(2010,1,1),end=c(2020,1,1))
actual_d_10 <- ts(project10['PM25'],frequency = 365,start=c(2010,1,1),end=c(2021,1,1))
plot.ts(PM25_d_10,type='o')

# adf test
adf.test(PM25_d_10)
#acf, pacf test
acf(PM25_d_10, 50)
pacf(PM25_d_10, 50)

fit <- stl(PM25_d_10, s.window="period")
plot(fit)

plot(PM25_d_10)
pmcomp_d <- decompose(PM25_d_10)
plot(pmcomp_d)

pmcomp_d_adjust <- PM25_d_10 - pmcomp_d$seasonal
plot(pmcomp_d_adjust)
plot(pmcomp_d$trend)

#fit arima model
(fit2 <- arima(PM25_d_10, c(2, 0, 0)))

#with diff
acf(diff(PM25_d_10), 50)
pacf(diff(PM25_d_10), 50)
(fit3 <- arima(PM25_d_10, c(0, 1, 4)))
(fit4 <- arima(PM25_d_10, c(0, 1, 3)))
(fit5 <- arima(PM25_d_10, c(0, 1, 2)))
(fit6 <- arima(PM25_d_10, c(0, 1, 1)))
(fit7 <- arima(PM25_d_10, c(0, 1, 5)))

#predict
autoplot(forecast(fit3, 365)) + autolayer(actual_d_10, alpha=0.5)

forcast_value_d = forecast(fit3, 365)[['mean']]
forcast_value_d = data.frame(forcast_value_d)
write.csv(forcast_value_d,file="/Users/apple/Documents/me/GWU/courses/6245/Project1/final/daily_forcast.csv",quote=F)


# Trend
lm_d <- lm(PM25_d_10$date~coredata(PM25_d_10))


# 12-20 daily data
PM25_d_12<-ts(project12['PM25'],frequency = 365,start=c(2012,1,1),end=c(2020,1,1))
actual_d_12 <- ts(project12['PM25'],frequency = 365,start=c(2012,1,1),end=c(2021,1,1))
plot.ts(PM25_d_12,type='o')

# adf test
adf.test(PM25_d_12)
#acf, pacf test
acf(PM25_d_12, 50)
pacf(PM25_d_12, 50)

#fit arima model
(fit22 <- arima(PM25_d_12, c(2, 0, 0)))

#with diff
acf(diff(PM25_d_12), 50)
pacf(diff(PM25_d_12), 50)
(fit32 <- arima(PM25_d_12, c(0, 1, 4)))
(fit42 <- arima(PM25_d_12, c(0, 1, 3)))
(fit52 <- arima(PM25_d_12, c(0, 1, 2)))
(fit62 <- arima(PM25_d_12, c(0, 1, 1)))

#predict
autoplot(forecast(fit32, 365)) + autolayer(actual_d_12, alpha=0.5)

# daily data trend
mk.test(project10$PM25, continuity = TRUE)
plot(project10$PM25, type = 'l',xlab = 'day',ylab='PM2.5')

