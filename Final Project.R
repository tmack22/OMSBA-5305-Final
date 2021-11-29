library(haven)
library(tidyverse)
library(fpp2)


housing_data <- read.csv("data/WASTHPI.csv")
TS <- ts(housing_data[,2], start =c(1975,1),frequency = 4)
plot(TS)
fit <- arima(TS)
print(summary(fit))
checkresiduals(fit)
#Model has clear upward trend. Normalize trend by taking difference
diffTS <- diff(TS)
plot(TS)
plot(diffTS)
acf(diffTS)
pacf(diffTS)
#Attempting to fit to ARIMA
arimadiff <- arima(diffTS)
print(summary(arimadiff))
checkresiduals(arimadiff)
#Attempting to fit to MA
madiff <- ma(diffTS, order = 4)
print(summary(madiff))
checkresiduals(madiff)

