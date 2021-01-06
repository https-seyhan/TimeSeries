# libraries
library(TTR)
library(Quandl)
library(fma)
library(car)
library(foreign)
library(tseries)
library(rugarch)
library(vars)
library(ets)

setwd("C:/Applied Business Forecasting")
cba_y <-read.csv("cba_1y.csv", sep=",", header=TRUE)
summary(cba_y)

cba_y.ts <- ts(cba_y$Last, freq = 7)
summary(cba_y.ts)

#ACF 
acf(cba_y.ts)



#PACF
pacf(cba_y.ts)

#Dickey-Fuller test of the time series
adf.test(cba_y.ts)

cba_y.ts.0diff1 <- diff(cba_y.ts, differences=1)
plot.ts(cba_y.ts.0diff1)
summary(cba_y.ts.0diff1)
        
        #ACF 
acf(cba_y.ts.0diff1 , lag.max= 30, main="ACF of First Difference Shares" )
adf.test(cba_y.ts.0diff1)

ARIMA.fitshare <- Arima(cba_y.ts, order=c(1,1,1)) 

acf(ARIMA.fitshare$res, main="ARIMA ACF(1,1,1)")
pacf(ARIMA.fitshare$res,  main="ARIMA PACF(1,1,1)")
summary(ARIMA.fitshare)

#SARIMA model
ARIMAfitShareSARIMA <- Arima(cba_y.ts, order=c(14,1,2), seasonal= list(order=c(1,1,1), period=7))
tsdisplay(residuals(ARIMAfitShareSARIMA),  main="SARIMA(30,1,1)(1,1,1) with 7 periods per season")

AIC(ARIMAfitShareSARIMA)


SARIMAfitShareResiduals <- residuals(ARIMAfitShareSARIMA)
tsdisplay(SARIMAfitShareResiduals)
Box.test(SARIMAfitShareResiduals, lag=30, fitdf=7, type="Ljung")

plot(forecast(ARIMAfitShareSARIMA, h=7))

forecast(ARIMAfitShareSARIMA, h=7)
