
install.packages("readxl")
install.packages("zoo")
install.packages("forecast")

library(readxl)
export <- read_excel("C:\\Users\\rache\\OneDrive\\Desktop\\Forecasting\\Net Exports India.xlsx")

#creating a time series for the trade balance data.
net_export.ts <-  ts(export$NX, start = c(1990,1), end = c(2022, 12), freq = 12)

#Average moving smoothing method
library(forecast)
library(zoo)
ma.trailing <- rollmean(net_export.ts, k = 12, align = "right")
ma.centered <- ma(net_export.ts, order = 12)
#Figure 5.2
plot(net_export.ts, ylim = c(-2300, 200), ylab = "NX", xlab = "Time", bty = "l", xaxt = "n",
     xlim = c(1990,2022.25), main = "")
axis(1, at = seq(1990, 2022.25, 1), labels = format(seq(1990, 2022.25, 1)))
lines(ma.centered, lwd = 2)
lines(ma.trailing, lwd = 2, lty = 2)
legend(1992,200, c("NX","Centered MA", "Trailing MA"), lty=c(1,1,2),
       lwd=c(1,2,2), bty = "n")

#Trailing moving average forecast with w = 12
nValid <- 60
nTrain <- length(net_export.ts) - nValid
train.ts <- window(net_export.ts, start = c(1990, 1), end = c(1990, nTrain))
valid.ts <- window(net_export.ts, start = c(1990, nTrain + 1), end = c(1990, nTrain + nValid))
ma.trailing <- rollmean(train.ts, k = 12, align = "right")
last.ma <- tail(ma.trailing, 1)
ma.trailing.pred <- ts(rep(last.ma, nValid), start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid), freq = 12)

#figure 5.3
plot(net_export.ts, ylim = c(-2300, 1000),  ylab = "NX", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1990,2025), main = "")
axis(1, at = seq(1990, 2025, 1), labels = format(seq(1990, 2025, 1)))
lines(ma.trailing, lwd = 2) 
lines(ma.trailing.pred, lwd = 2, col = "blue", lty = 2) 
lines(valid.ts)
lines(c(2023, 2023), c(-3000, 500))
lines(c(2017.25, 2017.25), c(-3000, 500))
text(2000.15, 500, "Training")
text(2020.25, 500, "Validation")
text(2025.25, 500, "Future")

# Forecasts from the Holt-Winter?s exponential smoothing
hwin <- ets(train.ts, model = "AAN")
hwin.pred <- forecast(hwin, h = nValid, level = 0)

#Figure 5.6
plot(hwin.pred, ylim = c(-2300, 500),  ylab = "NX", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1990,2025.25), main = "", flty = 2)
axis(1, at = seq(1990, 2025, 1), labels = format(seq(1990, 2025, 1)))
lines(hwin.pred$fitted, lwd = 1, col = "blue")
lines(valid.ts)
lines(c(2023, 2023), c(-2300, 500))
lines(c(2017.25, 2017.25), c(-2300, 500))
text(2000.15, 500, "Training")
text(2020.25, 500, "Validation")
text(2025.25, 500, "Future")

#accuracy
accuracy(hwin.pred, valid.ts)
accuracy(ma.trailing.pred,valid.ts)

