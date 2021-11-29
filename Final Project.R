library(haven)
library(tidyverse)
library(fpp2)
library(stats)
library(forecast)

housing_data <- read.csv("WASTHPI.csv")
TS <- ts(housing_data[,2], start =c(1975,1),frequency = 4)
log_TS <- log(TS)

plot(TS)
plot(log_TS)
fit <- arima(TS)
print(summary(fit))
checkresiduals(fit)


#Model has clear upward trend. Normalize trend by taking difference
diffTS <- diff(TS)
difflogTS <- diff(log_TS)
plot(TS)
plot(diffTS)
plot(difflogTS)
acf(difflogTS)
acf(diffTS)
pacf(diffTS)
pacf(difflogTS)

#seasonality
TScomp <- decompose(TS)
TSlogcomp <- decompose(log_TS)
TSseas <- stl(TS, s.window = "periodic")
plot(TScomp)
plot(TSlogcomp)
plot(TSseas)


#Attempting to fit to ARIMA
arimadiff <- arima(diffTS)
arimadifflog <- arima(difflogTS)
print(summary(arimadiff))
print(summary(arimadifflog))
checkresiduals(arimadiff)
checkresiduals(arimadifflog)

#Attempting to fit to MA
madiff <- ma(diffTS, order = 4)
madifflog <- ma(difflogTS, order = 4)
print(summary(madiff))
print(summary(madifflog))
checkresiduals(madiff)
checkresiduals(madifflog)

