install.packages("readxl")
install.packages("forecast")
install.packages("pastecs")

setwd("C:\\Users\\rache\\OneDrive\\Desktop\\Forecasting")


library(readxl)
library(forecast)
export <- read_excel("Net Exports India.xlsx")

#creating a time series for the trade balance data.
Net_export.ts <-  ts(export$NX, start = c(1990,1), end = c(2022, 12), freq = 12)
plot(Net_export.ts, xlab = "Time", ylab = "NX", ylim = c(-2300,200), bty = "l")

#A replicate of the figure 2.4 for the trade balance data.
Net_export.lm <- tslm(Net_export.ts ~ trend + I(trend^2))
par(mfrow = c(2, 1))
dev.off()
plot(Net_export.ts, xlab = "Time", ylab = "NX", ylim = c(-2300, 200), bty = "l")
lines(Net_export.lm$fitted, lwd = 2)
Net_export.ts.zoom <- window(Net_export.ts, start = c(2015, 1), end = c(2020, 12))
plot(Net_export.ts.zoom, xlab = "Time", ylab = "NX", ylim = c(-2300, 200), bty = "l")

library(pastecs)
summary_stats <- stat.desc(Export.data)
print(summary_stats)

#Split data
nValid <- 60
nTrain <- length(Net_export.ts) - nValid
train.ts <- window(Net_export.ts, start = c(1990, 1), end = c(1990, nTrain))
valid.ts <- window(Net_export.ts, start = c(1990, nTrain + 1), end = c(1990, nTrain + nValid))
Net_export.lm <-  tslm(train.ts ~ trend + I(trend^2))
Net_export.lm.pred <- forecast(Net_export.lm, h = nValid, level = 0)

# Figure 3-2
plot(Net_export.lm.pred, ylim = c(-2300, 200), ylab = "Net Exports", xlab = "Time", bty = "l",
     xaxt = "n", xlim = c(1990,2027.25), main = "", flty = 2)
axis(1, at = seq(1990, 2027, 1), labels = format(seq(1990, 2027, 1)))
lines(Net_export.lm$fitted, lwd = 2)
lines(valid.ts)


dev.off()
#Plot for quadratic forecast
plot(Net_export.lm.pred$residuals, ylim = c(-1500, 1000),  ylab = "Residuals", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1990,2027.25), main = "")
axis(1, at = seq(1990, 2027, 1), labels = format(seq(1990, 2027, 1)))
lines(valid.ts - Net_export.lm.pred$mean, lwd = 1)
lines(c(2022.25 - 5, 2022.25 - 5), c(-500, 3500))
lines(c(2022.25, 2022.25), c(-500, 3500))
text(2008.15, 500, "Training")
text(2020.25, 1000, "Validation")
text(2025.25, 500, "Future")

#computing predictive measures of quadratic forecast
accuracy(Net_export.lm.pred$mean, valid.ts)

#naive forecast
fixed.nValid <- 60
fixed.nTrain <- length(Net_export.ts) - fixed.nValid
train.ts <- window(Net_export.ts, start = c(1990, 1), end = c(1990, fixed.nTrain))
valid.ts <- window(Net_export.ts, start = c(1990, fixed.nTrain + 1), end = c(1990, fixed.nTrain + fixed.nValid))
naive.fixed <- naive(train.ts, h = fixed.nValid)
naive.roll <- ts(export$NX[fixed.nTrain:(fixed.nTrain + fixed.nValid - 1)], start = c(1990, fixed.nTrain + 1), end = c(1990, fixed.nTrain + fixed.nValid), freq = 12)

#predictive measures
accuracy(naive.fixed$mean, valid.ts)

#Plot histo residuals
hist(naive.fixed$residuals, ylab = "Frequency", xlab = "Forecast Error", bty = "l", main = "")

#plot residuals
plot(naive.fixed$residuals, ylim = c(-1500, 1000),  ylab = "Residuals", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1990,2027.25), main = "")
axis(1, at = seq(1990, 2027, 1), labels = format(seq(1990, 2027, 1)))
lines(valid.ts - naive.fixed$mean, lwd = 1)
lines(c(2022.25 - 5, 2022.25 - 5), c(-500, 3500))
lines(c(2022.25, 2022.25), c(-500, 3500))
text(2008.15, 500, "Training")
text(2020.25, 1000, "Validation")
text(2025.25, 500, "Future")


