library(haven)
library(tidyverse)
library(fpp2)
library(stats)
library(forecast)
#Pulling data in from excel export from FRED, converting to a TS
housing_data <- read.csv("WASTHPI.csv")
TS <- ts(housing_data[,2], start =c(1975,1),frequency = 4)
#Plotting Time Series
#Initial Observations:
#Data has a clear upward trend, along with two major dips. One around 1982 and one around 2010
plot(TS)
#Investigate data to confirm above observations
#There is a confirmed positive trend within the dataset. In addition there is also seasonlity which was not originally observed due to the scale of the dataset
TScomp <- stl(TS, s.window = "periodic")
plot(TScomp)
#Converting dataset to logithrithmic, and taking difference in order to remove seasonality and upward trend
log_TS <- log(TS)
difflogTS <- diff(log_TS)
plot(difflogTS)
#Confirming Seasonality and Upward Trend have been succesfully removed
plot(decompose(difflogTS))
#At this point we can begin looking for a best fit model
acf(difflogTS)
pacf(difflogTS)
#From our PACF's it seems we may be able to fit the model to a MA[2] function
madifflog <- ma(difflogTS, order = 2)
print(summary(madifflog))
checkresiduals(madifflog)
#After checking residuals of MA[2] function we found that there was still a signifigant portion of the dataset unexplained by the model
#We believe the large spikes in 1980 and 2008 are not able to be fully captured by a MA model alone. Therefore we are attempting to fit the dataset to an ARIMA model
arimadifflog <- auto.arima(difflogTS, stepwise = FALSE, approximation = FALSE)
print(summary(arimadifflog))
checkresiduals(arimadifflog)
#Forecasting using out model
arimadifflog %>% forecast(h=40)%>% autoplot()
