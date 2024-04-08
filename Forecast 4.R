install.packages("readxl")
install.packages("forecast")

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
#polynomial trend 
train.poly.trend<- tslm(train.ts ~ trend + I(trend^2))
train.poly.trend.pred <- forecast(train.poly.trend, h = nValid, level = 0)

#linear trend
train.linear.trend <- tslm(train.ts ~ trend)
train.linear.trend.pred <- forecast(train.linear.trend, h = nValid, level = 0)

#plot
plot(train.poly.trend.pred, ylim = c(-2300, 500), ylab = "Net_export", xlab = "Time", bty = "l",
      xaxt = "n", xlim = c(1990,2025.25), main = "", flty = 2)
axis(1, at = seq(1990, 2025, 1), labels = format(seq(1990, 2025, 1)))
lines(train.poly.trend.pred$fitted, lwd = 2, col = "blue")
lines(train.linear.trend.pred$fitted, lwd = 2, col = "black", lty = 3)
lines(train.linear.trend.pred$mean, lwd = 2, col = "black", lty = 3)
lines(valid.ts)
lines(c(2023, 2023), c(-3000, 500))
lines(c(2017.25, 2017.25), c(-3000, 500))
text(2000.15, 500, "Training")
text(2020.25, 500, "Validation")
text(2025.25, 500, "Future")

#accuracy
accuracy(train.linear.trend.pred,valid.ts)

accuracy(train.poly.trend.pred, valid.ts)
