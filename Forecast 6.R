

library(readxl)
data <- read_excel("C:\\Users\\rache\\OneDrive\\Desktop\\Forecasting\\Net Exports India.xlsx")

nTrain <- 336
yTrain.ts <- ts(data$NX[1:nTrain], freq = 12, start = c(1990, 1))
xTrain <- data.frame(exdata = data$S[1:nTrain],lagdata = data$lag_s[1:nTrain])                

nTest <- 60
xTest <- data.frame(exdata = data$S[(nTrain + 1):(nTrain + nTest)],lagdata = data$lag_s[(nTrain + 1):(nTrain + nTest)])

(formula <- as.formula(paste("yTrain.ts", paste(c("trend", colnames(xTrain)), collapse = "+"),
                             sep = "~")))
export.tslm <- tslm(formula, data = xTrain, lambda = 1)
export.tslm.pred <- forecast(export.tslm, newdata = xTest)

plot(export.tslm.pred, ylim = c(-1200,0),xlim = c(1990,2028), xlab = "Years", ylab = "Net exports")

plot(yTrain.ts, ylim = c(-1200,500),  ylab = "Net export", lwd = 2,
     xlab = "Date", bty = "l", xaxt = "n", xlim = c(1990,2023), main = "Forecast from Linear regression model with current and lagged exchange")
axis(1, at = seq(1990,2023, 1), labels = format(seq(1990,2023, 1)))
lines(export.tslm.pred$fitted, lwd = 2, col = "blue")
lines(export.tslm.pred$mean,lwd = 2,col = "blue",lty=3)
lines(c(2023, 2023), c(-3000, 500))
lines(c(2017.25, 2017.25), c(-3000, 500))
text(2000.15, 500, "Training")
text(2020.25, 500, "Validation")
text(2025.25, 500, "Future")

accuracy(export.tslm.pred,nTest)

#unit-root with drift -4.7
NX.ts <- ts(data$NX, start = c(1990,2), end = c(2022,12), freq = 12)
nv <- length(data$NX)
NXlag.ts <- ts(data$NX[1:nv-1], start = c(1990,1), end = c(2022,11), freq = 12)

# Estimate unit root with drift term 
train.trend<- tslm(yTrain.ts ~ trend + I(trend * -4.7) +NXlag.ts)
trend.res = resid(model_ts)

train.res.trend.arima <- Arima(trend.res, order = c(1,0,0))
train.res.trend.arima.pred <- forecast(train.res.trend.arima, h = 60)


accuracy(train.res.trend.arima.pred,60)
