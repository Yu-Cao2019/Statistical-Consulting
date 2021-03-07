# load packages
install.packages('forecast')
install.packages('tseries')
library(forecast)
library(tseries)

# 10-20monthly data
# load data
m_10<-read.csv(file="month_project10.csv")
PM25_m_10<-ts(m_10['PM25'],frequency = 12,start=c(2010,1),end=c(2019,12))
actual_m_10 <- ts(m_10['PM25'],frequency = 12,start=c(2010,1),end=c(2020,10))
plot.ts(PM25_m_10,type='o')

#seasonal effect
fit_s <- stl(PM25_m_10, s.window="period")
plot(fit_s)

plot(PM25_m_10)
pmcomp <- decompose(PM25_m_10)
plot(pmcomp)

pmcomp_adjust <- PM25_m_10 - pmcomp$seasonal
plot(pmcomp_adjust)
plot(pmcomp$trend)

# adf test
adf.test(PM25_m_10)
acf(PM25_m_10,50)
pacf(PM25_m_10,50)
# adf test with diff
adf.test(diff(PM25_m_10, alternative="stationary", k=0))
#acf, pacf test
acf(diff(PM25_m_10),50)
pacf(diff(PM25_m_10),50)

(fit_seasonal <- arima(PM25_m_10, c(0, 0, 1),seasonal = list(order = c(0, 0, 1), period = 12)))
(fit_seasonal2 <- arima(PM25_m_10, c(0, 0, 1),seasonal = list(order = c(0, 0, 2), period = 12)))
(fit_seasonal3 <- arima(PM25_m_10, c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12)))
(fit_seasonal4 <- arima(PM25_m_10, c(3, 1, 2),seasonal = list(order = c(1, 1, 1), period = 12)))
(fit_seasonal5 <- arima(PM25_m_10, c(3, 1, 2),seasonal = list(order = c(2, 1, 1), period = 12)))

autoplot(forecast(fit_seasonal3, 10)) + autolayer(actual_m_10)


forcast_value_m = forecast(fit_seasonal3, 10)[['mean']]
forcast_value_m = data.frame(forcast_value_m)
write.csv(forcast_value_m,file="/Users/apple/Documents/me/GWU/courses/6245/Project1/final/monthly_forcast.csv",quote=F)
accuracy(fit_seasonal3)


# 12-20monthly data
# load data
m_12<-read.csv(file="/Users/apple/Documents/me/GWU/courses/6245/Project1/final/month_project12.csv")
PM25_m_12<-ts(m_12['PM25'],frequency = 12,start=c(2012,1),end=c(2019,12))
actual_m_12 <- ts(m_12['PM25'],frequency = 12,start=c(2012,1),end=c(2020,10))
plot.ts(PM25_m_12,type='o')

#seasonal effect
fit_12 <- stl(PM25_m_12, s.window="period")
plot(fit_12)

# adf test
adf.test(PM25_m_12)
acf(PM25_m_12,50)
pacf(PM25_m_12,50)
# adf test with diff
adf.test(diff(PM25_m_12, alternative="stationary", k=0))
#acf, pacf test
acf(diff(PM25_m_12),50)
pacf(diff(PM25_m_12),50)

(fit_seasonal_12 <- arima(PM25_m_12, c(0, 0, 1),seasonal = list(order = c(0, 0, 1), period = 12)))
(fit_seasonal2_12 <- arima(PM25_m_12, c(3, 1, 2),seasonal = list(order = c(1, 1, 1), period = 12)))
(fit_seasonal3_12 <- arima(PM25_m_12, c(3, 1, 2),seasonal = list(order = c(1, 1, 2), period = 12)))

autoplot(forecast(fit_seasonal2_12, 10)) + autolayer(actual_m_12)

# monthly data trend
mk.test(m_10$PM25, continuity = TRUE)
plot(m_10$PM25, type = 'l',xlab = 'day',ylab='PM2.5')



