library(TTR)
library(Quandl)
library(fma)
library(car)
library(foreign)

setwd("C:/Applied Business Forecasting")

macrodata <- read.dta ("macro_data.dta")
summary(macrodata)
#Use this as the time series created.
plot(macrodata$cpi, macrodata$month)
#cpi time series
macrodata.ts.cpi <- ts(macrodata$cpi, start=c(1978,1), freq=12)
summary(macrodata.ts.cpi)
var(macrodata.ts.cpi)
sd(macrodata.ts.cpi)
plot.ts(macrodata.ts.cpi)

##Take the first difference of cpi.

inflation.ts <- diff(macrodata.ts.cpi, difference=1)*100 / 
plot(inflation.ts)

inflation.ts2 <- diff(macrodata.ts.cpi, difference=2)
plot(inflation.ts2)

#Decompose
inflation.ts.decomp <- decompose(inflation.ts, "multiplicative")
plot(inflation.ts.decomp)

#Remove resonal component.
inflation.ts.seasonallyadjusted <- inflation.ts - inflation.ts.decomp$seasonal
plot(inflation.ts.seasonallyadjusted)

#ACF PACF
acf(inflation.ts)

#ACF of Random component
acf(inflation.ts.decomp$random[7:426])
acf(inflation.ts.decomp$trend)
sd(inflation.ts[7:426])
sd(inflation.ts[7:426] - inflation.ts.decomp$trend[7:426] )
sd(inflation.ts.decomp$random[7:426])

#unemp time series
macrodata.ts.unemp <- ts(macrodata$unemp, start=c(1978,1), freq=12)
plot.ts(macrodata.ts.unemp)