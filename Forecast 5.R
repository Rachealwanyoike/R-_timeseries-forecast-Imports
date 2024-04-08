
install.packages("readxl")
install.packages("forecast")
install.packages("tseries")

library(readxl)
export <- read_excel("C:\\Users\\rache\\OneDrive\\Desktop\\Forecasting\\Net Exports India.xlsx")

#creating a time series for the trade balance data.
netexport.ts <-  ts(export$NX, start = c(1990,1), end = c(2022, 12), freq = 12)

#split data
nValid <- 60
nTrain <- length(netexport.ts) - nValid
train.ts <- window(netexport.ts, start = c(1990, 1), end = c(1990, nTrain))
valid.ts <- window(netexport.ts, start = c(1990, nTrain + 1), end = c(1990, nTrain + nValid))

library(forecast)
#linear trend
train.linear.trend <- tslm(train.ts ~ trend)
summary(train.linear.trend)

#plot residual autocorrelation
Acf(train.linear.trend$residuals, lag.max = 12, main = "")$acf

#Fit AR(1) model to residual series.
train.res.arima <- Arima(train.linear.trend$residuals, order = c(1,0,0))
train.res.arima.pred <- forecast(train.res.arima, h = nValid)
plot(train.linear.trend$residuals, ylim = c(-500, 500), ylab = "Residuals",
     xlab = "Time", bty = "l", xaxt = "n", xlim = c(1990,2025.25), main = "")
axis(1, at = seq(1990, 2025, 1), labels = format(seq(1990, 2025, 1)))
lines(train.res.arima.pred$fitted, lwd = 2, col = "blue")
lines(c(2023, 2023), c(-500, 500))
lines(c(2017.75, 2017.75), c(-500, 500))
text(2000.15, 500, "Training")
text(2020.25, 500, "Validation")
text(2025.25, 500, "Future")

summary(train.res.arima)

#2.

Acf(train.res.arima$residuals, lag.max = 12, main = "")$acf

#Fit AR(1) term model to residual series.
train.res.arima1 <- Arima(train.res.arima$residuals, order = c(1,0,0))
train.res.arima1.pred <- forecast(train.res.arima1, h = nValid)
plot(train.res.arima$residuals, ylim = c(-500, 500), ylab = "Residuals",
     xlab = "Time", bty = "l", xaxt = "n", xlim = c(1990,2025.25), main = "")
axis(1, at = seq(1990, 2025, 1), labels = format(seq(1990, 2025, 1)))
lines(train.res.arima1.pred$fitted, lwd = 2, col = "blue")
lines(c(2023, 2023), c(-500, 500))
lines(c(2017.75, 2017.75), c(-500, 500))
text(2000.15, 500, "Training")
text(2020.25, 500, "Validation")
text(2025.25, 500, "Future")

#adf test
library(tseries)
adf.test(x=netexport.ts,alternative = "stationary", k=12)
