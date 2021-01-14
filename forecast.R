install.packages("quantmod")
install.packages("tseries")
install.packages("timeSeries")
install.packages("forecast")
install.packages("xts")

library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)

getSymbols("SPY", from = "2014-01-01", to = "2021-01-07") #xts/zoo time series data frames (regular tidyverse preprocessing does not work)
closing <- SPY[,4] #selecting the 4th column of the set since closing price is the only important value

#plot closing value over time
plot(closing)

#ACF and PACF plots
par(mfrow = c(1,2))
acf(closing, main = "ACF plot")
pacf(closing, main = "PACF plot")

#differencing to lower p value down to less than 5% if needed
adf.test(closing) #adf test to ensure stationary data

fit1 <- auto.arima(closing, seasonal = FALSE) #automatic parameters
tsdisplay(residuals(fit1), lag.max = 40, main = "(0,1,2) fit")

#plotting both prediction and previous years in same plot
par(mfrow = c(1,1))
plot(as.ts(closing), main = "fit1")
lines(fitted(fit1), col = "blue")

fit2 <- arima(closing, order  = c(1,1,28)) #(1,1,28) parameter
tsdisplay(residuals(fit2), lag.max = 40, main = "(1,1,28) fit")

#plotting both prediction and previous years in same plot
plot(as.ts(closing), main = "fit2")
lines(fitted(fit2), col = "blue")

fit3 <- arima(closing, order  = c(1,1,37)) #(1,1,28) parameter
tsdisplay(residuals(fit3), lag.max = 40, main = "(1,1,37) fit")

#plotting both prediction and previous years in same plot
plot(as.ts(closing), main = "fit3")
lines(fitted(fit3), col = "blue")

fit4 <- arima(closing, order  = c(1,1,39)) #(1,1,39) parameter
tsdisplay(residuals(fit4), lag.max = 40, main = "(1,1,39) fit")

#plotting both prediction and previous years in same plot
plot(as.ts(closing), main = "fit4")
lines(fitted(fit4), col = "blue")

days <- 1
par(mfrow = c(2,2))
forecast1 <- forecast(fit1, h = days)
plot(forecast1)
forecast2 <- forecast(fit2, h = days)
plot(forecast2)
forecast3 <- forecast(fit3, h = days)
plot(forecast3)
forecast4 <- forecast(fit4, h = days)
plot(forecast4)

forecast1
forecast2
forecast3
forecast4

#testing accuracy
accuracy(forecast1)
accuracy(forecast2)
accuracy(forecast3)
accuracy(forecast4)






