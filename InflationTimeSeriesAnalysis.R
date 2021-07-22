# Libraries
library(TTR)
library(Quandl)
library(fma)
library(car)
library(foreign)
library(tseries)
library(rugarch)
library(vars)

setwd("C:/Applied Business Forecasting/Assignment")
macrodata <-read.csv("macro_data.csv", sep=",", header=TRUE)
summary(macrodata)

macrodata.ts.cpi <- ts(macrodata$cpi, start=c(1978,1), freq=12)
macrodata.ts.cpi.decomp <- decompose(macrodata.ts.cpi, "additive")
plot(macrodata.ts.cpi.decomp)

#The Inflation data series
macrodata.ts.inflation <- ts(macrodata$inflation, start=c(1978,1), freq=12)
New.Inflation <- time(macrodata.ts.inflation)
plot.ts(macrodata.ts.inflation); abline(reg=lm(macrodata.ts.inflation ~ New.Inflation ))
var(macrodata.ts.inflation)
sd(macrodata.ts.inflation )
plot.ts(macrodata.ts.inflation)
plot(stl(macrodata.ts.inflation, 'periodic'))

#Decompose Analysis
inflation.ts.decomp <- decompose(macrodata.ts.inflation, "mult")
plot(inflation.ts.decomp)

inflation.ts.decomp <- decompose(macrodata.ts.inflation, "additive")
plot(inflation.ts.decomp)

Trend <- inflation.ts.decomp$trend
Seasonal <- inflation.ts.decomp$seasonal
ts.plot(cbind(Trend, Trend + Seasonal), lty = 1:2) # Additive
var(inflation.ts.decomp$trend)
var(inflation.ts.decomp$seasonal)
var(inflation.ts.decomp$random)

#Dickey-Fuller test
adf.test(macrodata.ts.inflation)

#Remove seasonal component.
inflation.ts.seasonallyadjusted <- macrodata.ts.inflation - inflation.ts.decomp$seasonal
plot(inflation.ts.seasonallyadjusted)
sd(inflation.ts.seasonallyadjusted)

#ACF 
acf(inflation.ts.seasonallyadjusted)

#Remove trend component.
inflation.ts.trendadjusted <- macrodata.ts.inflation - inflation.ts.decomp$trend
              plot(inflation.ts.trendadjusted)
sd(inflation.ts.trendadjusted)

New.inflation.ts.trendadjusted  <- time(inflation.ts.trendadjusted )
plot.ts(inflation.ts.trendadjusted); abline(reg=lm(inflation.ts.trendadjusted ~ New.inflation.ts.trendadjusted  ))
summary(inflation.ts.trendadjusted)

#ACF PACF
acf(inflation.ts.trendadjusted[12:426])
pacf(inflation.ts.trendadjusted[12:426])

inflation.ts.seasonallyadjusted.decomp <- decompose(inflation.ts.seasonallyadjusted, "additive")
plot(inflation.ts.seasonallyadjusted.decomp)

#Remove Trend and seasonality
inflation.ts.trendseasonadjusted <- macrodata.ts.inflation - inflation.ts.decomp$trend - inflation.ts.decomp$seasonal
plot(inflation.ts.trendseasonadjusted)
summary(inflation.ts.trendseasonadjusted)
sd(inflation.ts.trendseasonadjusted[13:426]) #removing trend is effective

#ACF PACF
acf(inflation.ts.trendseasonadjusted[12:426], main= "Inflation Series Trend & Seasonality Adjusted")
pacf(inflation.ts.trendseasonadjusted[12:426], main="Inflation Series Trend & Seasonality Adjusted")

#Dickey-Fuller test
#he null hypothesis is that there is a unit root, δ = 0 {\displaystyle \delta =0} \delta = 0. 
adf.test(inflation.ts.trendseasonadjusted[12:426])
adf.test(inflation.ts.trendseasonadjusted[12:426], k=1)

acf(inflation.ts.trendseasonadjusted[12:426], lag.max = 60)
#ACF PACF
acf(macrodata.ts.inflation, lag.max = 60)
pacf(macrodata.ts.inflation, main="Inflation Series")

#MODELS For Inflation#
#ARMA##############################################################
fitInflationARMA <- Arima(macrodata.ts.inflation, order=c(1,0,1))
summary(fitInflationARMA)
Box.test(residuals(fitInflationARMA ), lag=24, fitdf=4, type="Ljung")

fitInflationARMA <- Arima(macrodata.ts.inflation, order=c(2,0,2))
summary(fitInflationARMA)
Box.test(residuals(fitInflationARMA ), lag=24, fitdf=4, type="Ljung")

plot(forecast(fitInflationARMA ))
forecast(fitInflationARMA ) #print out forecasts and 95% intervals

fitInflationARIMA <- Arima(macrodata.ts.inflation, order=c(12,0,2))
summary(fitInflationARIMA)
Box.test(residuals(fitInflationARIMA ), lag=24, fitdf=4, type="Ljung")

#ARIMA##############################################################
fitInflationARIMA <- Arima(macrodata.ts.inflation, order=c(12,1,2))
summary(fitInflationARIMA)
Box.test(residuals(fitInflationARIMA ), lag=24, fitdf=4, type="Ljung")

fitInflationARIMA  <- Arima(macrodata.ts.inflation, order=c(13,1,3)) 
summary(fitInflationARIMA)
Box.test(residuals(fitInflationARIMA ), lag=24, fitdf=4, type="Ljung")

fitInflationARIMA <- Arima(macrodata.ts.inflation, order=c(13,1,4)) #
summary(fitInflationARIMA)
Box.test(residuals(fitInflationARIMA ), lag=24, fitdf=4, type="Ljung")

fitInflationARIMA <- Arima(macrodata.ts.inflation, order=c(24,1,4)) #
summary(fitInflationARIMA)
Box.test(residuals(fitInflationARIMA ), lag=24, fitdf=4, type="Ljung")

acf(fitInflationARIMA$res[13:415], main="ACF of ARIMA")

plot(forecast(fitInflationARIMA))
forecast(fitInflationARIMA) #prin out forecasts and 95% intervals

################################################################################
rawinflation.ar<-ar(macrodata.ts.inflation, method="mle")
rawinflation.ar
plot(rawinflation.ar$res)

acf(rawinflation.ar$res[13:415])
pacf(rawinflation.ar$res[12:426])

#mean
mean(macrodata.ts.inflation)

#Order of AR
rawinflation.ar$order

#There is no observation before t=13
acf(rawinflation.ar$res[13:415])
acf(rawinflation.ar$res[-(1:rawinflation.ar$order)])

#SARIMA MODEL###########################################################
fitInflationSARIMA <- Arima(macrodata.ts.inflation, order=c(1,1,1), seasonal= list(order=c(1,1,1), period=12))
tsdisplay(residuals(fitInflationSARIMA),  main="SARIMA(1,1,1)(1,1,1) with 7 periods per season")

summary(fitInflationSARIMA)

fitInflationSARIMA <- Arima(macrodata.ts.inflation, order=c(12,1,1), seasonal= list(order=c(12,1,1), period=12))
tsdisplay(residuals(fitInflationSARIMA),  main="SARIMA(12,1,1)(12,1,1) with 12 periods per season")

Box.test(residuals(fitInflationSARIMA), lag=24, fitdf=7, type="Ljung")
summary(fitInflationSARIMA)

fitInflationSARIMA <- Arima(macrodata.ts.inflation, order=c(24,1,2), seasonal= list(order=c(1,1,1), period=12))
tsdisplay(residuals(fitInflationSARIMA),  main="SARIMA(24,1,2)(1,1,1) with 12 periods per season")

Box.test(residuals(fitInflationSARIMA), lag=24, fitdf=7, type="Ljung")
summary(fitInflationSARIMA)

fitInflationSARIMA
plot(forecast(fitInflationSARIMA , h=12))

forecast(fitInflationSARIMA, h=1) #forecast for April 2014
forecast(fitInflationSARIMA, h=12)

########################PART 2########################
browsers <-read.csv("browsermarket01.csv", sep=",", header=TRUE)

#make time series data
browsers.ts.Internet.Explorer.8.0 <- ts(browsers$Internet.Explorer.8.0, freq = 7)
summary(browsers.ts.Internet.Explorer.8.0)

#ACF 
acf(browsers.ts.Internet.Explorer.8.0 )

#Dickey-Fuller test of the time series
adf.test(browsers.ts.Internet.Explorer.8.0)

browsers.ts.Internet.Explorer.8.0diff1 <- diff(browsers.ts.Internet.Explorer.8.0, differences=1)
plot.ts(browsers.ts.Internet.Explorer.8.0diff1)
summary(browsers.ts.Internet.Explorer.8.0diff1)

#ACF 
acf(browsers.ts.Internet.Explorer.8.0diff1, lag.max= 30, main="ACF of First Difference Internet Explorer 8 Market Share" )
adf.test(browsers.ts.Internet.Explorer.8.0diff1)

#ACF refuses to die out and peristetly high. So. take first difference.
#So, take the first difference
browsers.ts.Internet.Explorer.8.0diff1 <- diff(browsers.ts.Internet.Explorer.8.0, differences=1)
plot.ts(browsers.ts.Internet.Explorer.8.0)

#Additive Decomposition of Explorer 8.0
browsers.ts.Internet.Explorer.8.0.decomp <- decompose(browsers.ts.Internet.Explorer.8.0, "additive")
plot(browsers.ts.Internet.Explorer.8.0.decomp)

#Multiplicative Decomposition of Explorer 8.0
browsers.ts.Internet.Explorer.8.0.decomp <- decompose(browsers.ts.Internet.Explorer.8.0, "mult")
plot(browsers.ts.Internet.Explorer.8.0.decomp)

New.browser8.0 <- time(browsers.ts.Internet.Explorer.8.0.decomp)
plot.ts(browsers.ts.Internet.Explorer.8.0); abline(reg=lm(browsers.ts.Internet.Explorer.8.0 ~ New.browser8.0))
var(New.browser8.0 )

plot(stl(browsers.ts.Internet.Explorer.8.0, 'periodic'))

#Remove trend component.
browsers.ts.Internet.Explorer.8.0.trendadjusted <- browsers.ts.Internet.Explorer.8.0 - browsers.ts.Internet.Explorer.8.0.decomp$trend
plot(browsers.ts.Internet.Explorer.8.0)
plot(browsers.ts.Internet.Explorer.8.0.trendadjusted)
sd(browsers.ts.Internet.Explorer.8.0.trendadjusted)

#Remove seasonal component
browsers.ts.Internet.Explorer.8.0.seasonjusted <- browsers.ts.Internet.Explorer.8.0 - browsers.ts.Internet.Explorer.8.0.decomp$season
plot(browsers.ts.Internet.Explorer.8.0)
plot(browsers.ts.Internet.Explorer.8.0.seasonjusted)
sd(browsers.ts.Internet.Explorer.8.0.seasonjusted)

#Remove Trend and seasonality
browsers.ts.Internet.Explorer.8.0.trendseasonadjusted <- browsers.ts.Internet.Explorer.8.0 - browsers.ts.Internet.Explorer.8.0.decomp$trend - browsers.ts.Internet.Explorer.8.0.decomp$seasonal
plot(browsers.ts.Internet.Explorer.8.0.trendseasonadjusted)
summary(browsers.ts.Internet.Explorer.8.0.trendseasonadjusted)
sd(browsers.ts.Internet.Explorer.8.0.trendseasonadjusted[6:1365]) 

#Multiplicative Depomposition produces less variance. So better to use them

#ACF 
acf(browsers.ts.Internet.Explorer.8.0, lag.max=30 )
acf(browsers.ts.Internet.Explorer.8.0.trendseasonadjusted[6:1365])

#Decompose First Difference Data

#Additive Decomposition of Explorer 8.0
browsers.ts.Internet.Explorer.8.0diff1.decomp <- decompose(browsers.ts.Internet.Explorer.8.0diff1, "additive")
plot(browsers.ts.Internet.Explorer.8.0diff1.decomp)

#Multiplicative Decomposition of Explorer 8.0
browsers.ts.Internet.Explorer.8.0diff1.decomp <- decompose(browsers.ts.Internet.Explorer.8.0diff1, "mult")
plot(browsers.ts.Internet.Explorer.8.0diff1.decomp)

#Remove trend component.
browsers.ts.Internet.Explorer.8.0diff1.trendadjusted <- browsers.ts.Internet.Explorer.8.0diff1- browsers.ts.Internet.Explorer.8.0diff1.decomp$trend
plot(browsers.ts.Internet.Explorer.8.0diff1)
plot(browsers.ts.Internet.Explorer.8.0diff1.trendadjusted)
sd(browsers.ts.Internet.Explorer.8.0diff1.trendadjusted)

#Remove seasonal component
browsers.ts.Internet.Explorer.8.0diff1.seasonjusted <- browsers.ts.Internet.Explorer.8.0diff1 - browsers.ts.Internet.Explorer.8.0diff1.decomp$season
plot(browsers.ts.Internet.Explorer.8.0diff1)
plot(browsers.ts.Internet.Explorer.8.0diff1.seasonjusted)
sd(browsers.ts.Internet.Explorer.8.0.seasonjusted)

###################################################################################
#MODELs FOR PART 2
ARIMA.fitExplorer8 <- Arima(browsers.ts.Internet.Explorer.8.0diff1, order=c(1,1,1)) 

acf(ARIMA.fitExplorer8$res, main="ARIMA ACF(1,1,1)")
pacf(ARIMA.fitExplorer8$res,  main="ARIMA PACF(1,1,1)")
summary(ARIMA.fitExplorer8)

ARIMA.fitExplorer8 <- Arima(browsers.ts.Internet.Explorer.8.0diff1, order=c(1,1,30)) 

summary(ARIMA.fitExplorer8)

Acf(residuals(ARIMA.fitExplorer8), main="ARIMA ACF(1,1,30)")

acf(ARIMA.fitExplorer8$res, main="ARIMA ACF(1,1,30)")
pacf(ARIMA.fitExplorer8$res,  main="ARIMA PACF(1,1,30)")

Acf(residuals(ARIMA.fitExplorer8), main="ARIMA ACF(1,1,30)")
Box.test(residuals(ARIMA.fitExplorer8), lag=24, fitdf=4, type="Ljung")

plot(forecast(ARIMA.fitExplorer8))
forecast(ARIMA.fitExplorer8) #print out forecasts and 95% intervals

#use seasonaly removed first difference data
ARIMA.fitExplorer8 <- Arima(browsers.ts.Internet.Explorer.8.0diff1.seasonjusted, order=c(1,1, 30)) 
summary(ARIMA.fitExplorer8)

Acf(residuals(ARIMA.fitExplorer8), main="ARIMA ACF(1,1,30)")

acf(ARIMA.fitExplorer8$res, main="ARIMA ACF(1,1,30)")
pacf(ARIMA.fitExplorer8$res,  main="ARIMA PACF(1,1,30)")

ARIMA.fitExplorer8 <- Arima(browsers.ts.Internet.Explorer.8.0diff1, order=c(1,1,7)) 
summary(ARIMA.fitExplorer8)

Acf(residuals(ARIMA.fitExplorer8), main="ARIMA ACF(1,1,7)")
acf(ARIMA.fitExplorer8$res, main="ARIMA ACF(1,1,7)")
pacf(ARIMA.fitExplorer8$res,  main="ARIMA PACF(1,1,7)")

#SARIMA Model
ARIMAfitExplorer8SARIMA <- Arima(browsers.ts.Internet.Explorer.8.0diff1, order=c(1,1,1), seasonal= list(order=c(1,1,1), period=7))
tsdisplay(residuals(ARIMAfitExplorer8SARIMA),  main="SARIMA(1,1,1)(1,1,1) with 7 periods per season")

AIC(ARIMAfitExplorer8SARIMA <- Arima(browsers.ts.Internet.Explorer.8.0diff1, order=c(1,1,1), seasonal= list(order=c(1,1,1), period=7)))

ARIMAfitExplorer8SARIMA <- Arima(browsers.ts.Internet.Explorer.8.0diff1, order=c(1,1,30), seasonal= list(order=c(1,1,1), period=7))
tsdisplay(residuals(ARIMAfitExplorer8SARIMA), main="SARIMA(1,1,30)(1,1,1) with 7 periods per season")

AIC(Arima(browsers.ts.Internet.Explorer.8.0diff1, order=c(1,1,30), seasonal= list(order=c(1,1,1), period=7)))

fitExplorer8SARIMA <- Arima(browsers.ts.Internet.Explorer.8.0diff1, order=c(1,1,30), seasonal= list(order=c(1,1,1), period=7))

SARIMAExplorer8Residuals <- residuals(fitExplorer8SARIMA)
tsdisplay(SARIMAExplorer8Residuals)
Box.test(SARIMAExplorer8Residuals, lag=30, fitdf=7, type="Ljung")

plot(forecast(fitExplorer8SARIMA, h=120))

forecast(fitExplorer8SARIMA, h=7)

#VAR MODEL##################################################
browsers.ts.other <- ts(browsers$Other, freq = 7)

#take the differences of other internet bowsers
browsers.ts.other.diff1 <- diff(browsers.ts.other, differences=1)

plot(browsers.ts.other.diff1)

VARselect(cbind(browsers.ts.Internet.Explorer.8.0diff1,browsers.ts.other.diff1), lag.max = 30, type = "trend")$selection
BrowserVAR <- VAR(cbind(browsers.ts.Internet.Explorer.8.0,browsers.ts.other) , p=16, type="trend")

#Perform serial test to choose rigth p value.
serial.test(BrowserVAR , lags.pt=30, type="PT.asymptotic") #we don't reject AR(16)
acf(resid(BrowserVAR )[,1], main="Explorer 8 VAR(16)")
acf(resid(BrowserVAR )[,2], main="Other Internet Browsers VAR(16)")
summary(BrowserVAR )

coef(BrowserVAR$ )

resid(BrowserVAR)
fcst <- forecast(BrowserVAR, n.ahead=90 )
plot(fcst, xlab="Time in Days")
fcst
fcst

#Get MSE on the Training Data
####################################################################
browsers <-read.csv("browsermarket01Training.csv", sep=",", header=TRUE)

#make time series data
browsers.ts.Internet.Explorer.8.0 <- ts(browsers$Internet.Explorer.8.0, freq = 7)
summary(browsers.ts.Internet.Explorer.8.0)

#ACF 
acf(browsers.ts.Internet.Explorer.8.0 )

#Dickey-Fuller test of the time series
adf.test(browsers.ts.Internet.Explorer.8.0)

browsers.ts.Internet.Explorer.8.0diff1 <- diff(browsers.ts.Internet.Explorer.8.0, differences=1)
browsers.ts.other <- ts(browsers$Other, freq = 7)

#take the differences of other internet bowsers
browsers.ts.other.diff1 <- diff(browsers.ts.other, differences=1)

BrowserVAR_Training <- VAR(cbind(browsers.ts.Internet.Explorer.8.0,browsers.ts.other) , p=16, type="trend")

fcst_Training <- forecast(BrowserVAR_Training, n.ahead=7 )
plot(fcst_Training, xlab="Time in Days")
fcst_Training

#ARCH MODEL######################
browsers <-read.csv("browsermarket01.csv", sep=",", header=TRUE)

#make time series data
browsers.ts.Internet.Explorer.8.0 <- ts(browsers$Internet.Explorer.8.0, freq = 7)
summary(browsers.ts.Internet.Explorer.8.0)

browsers.ts.Internet.Explorer.8.0diff1 <- diff(browsers.ts.Internet.Explorer.8.0, differences=1)
plot.ts(browsers.ts.Internet.Explorer.8.0diff1)

#Check If there is Conditional Heteroskeasticity
acf(browsers.ts.Internet.Explorer.8.0diff1 - mean(browsers.ts.Internet.Explorer.8.0diff1)^2, main="Squared Mean Adjusted Explorer 8")

#Model GARCH

GARCHbrowsers.ts.Internet.Explorer.8.0 <- garch(browsers.ts.Internet.Explorer.8.0diff1, order=c(0,2))
#Obtain confidence intervals of coeffiencts of fitted model
t(confint(GARCHbrowsers.ts.Internet.Explorer.8.0))

summary(GARCHbrowsers.ts.Internet.Explorer.8.0)

GARCHbrowsers.ts.Internet.Explorer.8.0.res <- resid(GARCHbrowsers.ts.Internet.Explorer.8.0)[-1]

acf(GARCHbrowsers.ts.Internet.Explorer.8.0.res[5:1366], main="Residuals from the GARCH(0,2) Model")
acf(GARCHbrowsers.ts.Internet.Explorer.8.0.res[5:1366]^2, main="Residual Squares from the GARCH(0,2) Model")

GARCHbrowsers.ts.Internet.Explorer.8.0 <- garch(browsers.ts.Internet.Explorer.8.0diff1, order=c(1,1))

#Obtain confidence intervals of coeffiencts of fitted model
t(confint(GARCHbrowsers.ts.Internet.Explorer.8.0))

summary(GARCHbrowsers.ts.Internet.Explorer.8.0)

GARCHbrowsers.ts.Internet.Explorer.8.0.res <- resid(GARCHbrowsers.ts.Internet.Explorer.8.0)[-1]

acf(GARCHbrowsers.ts.Internet.Explorer.8.0.res[5:1366], main="Residuals from the GARCH Model")




acf(GARCHbrowsers.ts.Internet.Explorer.8.0.res[5:1366]^2, main="Residual Squares from the GARCH Model")


#Get thre predicted values on Testdata to get the MSEs of ARIMA, SARIMA, VAR
#Get MSE on the Training Data
####################################################################

#ARIMA Forecast

browsers <-read.csv("browsermarket01Training.csv", sep=",", header=TRUE)

#make time series data

browsers.ts.Internet.Explorer.8.0_Training <- ts(browsers$Internet.Explorer.8.0, freq = 7)


ARIMA.fitExplorer8_Training <- Arima(browsers.ts.Internet.Explorer.8.0_Training, order=c(1,1,30)) 

summary(ARIMA.fitExplorer8_Training)

fcst_Training <- forecast(ARIMA.fitExplorer8_Training, n.ahead=7 )
plot(fcst_Training, xlab="Time in Days")
fcst_Training

#SARIMA Forecast

fitExplorer8SARIMA_Training <- Arima(browsers.ts.Internet.Explorer.8.0_Training, order=c(1,1,30), seasonal= list(order=c(1,1,1), period=7))
SARIMAExplorer8Residuals <- residuals(fitExplorer8SARIMA_Training)
tsdisplay(SARIMAExplorer8Residuals)
summary(fitExplorer8SARIMA_Training)

plot(forecast(fitExplorer8SARIMA_Training, h=10))

forecast(fitExplorer8SARIMA_Training, h=10)

#VAR Forecast


browsers <-read.csv("browsermarket01Training.csv", sep=",", header=TRUE)

#make time series data

browsers.ts.Internet.Explorer.8.0 <- ts(browsers$Internet.Explorer.8.0, freq = 7)
summary(browsers.ts.Internet.Explorer.8.0)



browsers.ts.Internet.Explorer.8.0diff1 <- diff(browsers.ts.Internet.Explorer.8.0, differences=1)

browsers.ts.other <- ts(browsers$Other, freq = 7)

#take the differences of other internet bowsers
browsers.ts.other.diff1 <- diff(browsers.ts.other, differences=1)

BrowserVAR_Training <- VAR(cbind(browsers.ts.Internet.Explorer.8.0,browsers.ts.other) , p=16, type="trend")


fcst_Training <- forecast(BrowserVAR_Training, n.ahead=7 )
plot(fcst_Training, xlab="Time in Days")
fcst_Training

#Output the prediction of best model(SARIMA)

fitfinal <-  Arima(browsers.ts.Internet.Explorer.8.0, order=c(1,1,30), seasonal= list(order=c(1,1,1), period=7))
SARIMAExplorer8Residuals <- residuals(fitfinal)
tsdisplay(SARIMAExplorer8Residuals)
summary(fitfinal)


AIC(Arima(browsers.ts.Internet.Explorer.8.0, order=c(1,1,30), seasonal= list(order=c(1,1,1), period=7)))
Box.test(SARIMAExplorer8Residuals, lag=30, fitdf=7, type="Ljung")

plot(forecast(fitfinal , h=90), main = "90 days Forecasts from SARIMA(1,1,30)(1,1,1)[7]", ylab= "Internet Explorer 8 Market share %", xlab="Time Intervals in days")

forecast(fitfinal, h=15)
