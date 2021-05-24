library(tseries)
library(astsa)
BTC <- read.csv("P6BTC.csv")

price <- data.frame(BTC$Close)

colnames(price) <- "btc"

price.new <- price[!price$btc=="null",]

log.price.new <- log(as.numeric(price.new))

log.return.new <- diff(log.price.new)

which.min(log.return.new)

min(log.return.new)

plot.ts(log.return.new, main ="Daily log-return from 2014-09-17 to 2021-04-13", 
        ylab = "log-return", xlab = "Day")

price.d <- data.frame(price[seq(1933, 2401),] ) #daily from 2020-01-01 to 2021-04-13

day <- data.frame(BTC$Date)

day <- day[seq(1933, 2401),]

colnames(price.d) <- "DailyPrices"

price.d <- price.d[!price.d$DailyPrices=="null",]

price.d <- ts(as.numeric(price.d))

plot(price.d)

adf.test(price.d)
adf.test(price.d, k=0)

log.price.d <- log(price.d)

plot(log.price.d, ylab = "log-price")

adf.test(log.price.d)
adf.test(log.price.d, k=0)

log.return <- ts(diff(log.price.d)[-71])

plot(log.return, ylab = "log-return")

which.min(log.return)
min(log.return)
# There is a very ekstreem return (-0.465) at day 71: 2020-03-11
# This is considered to be an outlier -> it is removed

plot(log.return, ylab = "log-return",
     main = "Daily log-returns from 2020-01-01 to 2021-04-13")

adf.test(log.return)
adf.test(log.return, k=0)

mean(log.return)

sd(log.return)

acf2(log.return, max.lag = 100, main = "Correlogram of log-returns")

#Model 0 (mean model)

mean.model <- lm(log.return~1)
summary(mean.model)

aic0 <- AIC(mean.model)
bic0 <- BIC(mean.model)

#Model 1: ARMA(1,0)
ar1 <- lm(log.return[-1]~log.return[-461])
summary(ar1)

aic1 <- AIC(ar1)
bic1 <- BIC(ar1)

#Model 2: ARMA(2,0)
ar2 <- lm(log.return[c(-1, -2, -461)]~log.return[c(-1, -460,-461)]
          +log.return[c(-459, -460, -461)])
summary(ar2)
aic2 <- AIC(ar2)
bic2 <- BIC(ar2)

#Model 3: ARMA(0,1)
ma1 <- arima(log.return, order = c(0,0,1))
aic3 <- AIC(ma1)
bic3 <- BIC(ma1)
#Model 4: ARMA(0,2)
ma2 <- arima(log.return, order = c(0,0,2))
aic4 <- AIC(ma2)
bic4 <- BIC(ma2)
#Model 5: ARMA(1,1)
arma11 <- arima(log.return, order = c(1,0,1))
aic5 <- AIC(arma11)
bic5 <- BIC(arma11)
#Model 6: ARMA(2,1)
arma21 <- arima(log.return, order = c(2,0,1))
aic6 <- AIC(arma21)
bic6 <- BIC(arma21)
#Model 7: ARMA(1,2)
arma12 <- arima(log.return, order = c(1,0,2))
aic7 <- AIC(arma12)
bic7 <- BIC(arma12)
#Model 8: ARMA(2,2)
arma22 <- arima(log.return, order = c(2,0,2))
aic8 <- AIC(arma22)
bic8 <- BIC(arma22)

aic <- c(aic1, aic2, aic3, aic4, aic5, aic6, aic7, aic8)
bic <- c(bic1, bic2, bic3, bic4, bic5, bic6, bic7, bic8)
plot(aic, type="b", ylim = c(-1779, -1745), ylab = "Information Criteria", 
     xlab = "Model", col = "red", lwd=2)
points(bic, type="b", col= "blue", lwd =2)
legend(7, -1756, legend = c("BIC", "AIC"), col=c("blue", "red"),
       pch = c(1,1), pt.lwd = c(2,2))

which.min(aic)
which.min(bic)

aic[3] # ARMA(0,1)
aic0
# Mean model is best according to AIC

bic[3] #ARMA(0,1)
bic0
# Mean model is best according to BIC

mean(residuals(ma1)^2)
mean(residuals(mean.model)^2)

#But ma1 is best according to MSE

# ARMA 

plot(ts(log.return), ylab = "log-return", main ="Fitted MA(1) model to log-returns")
fit.ma1 <- log.return - residuals(ma1)

abline(h=mean(log.return))
points(fit.ma1, type="l", col="blue")

res.ma1 <- residuals(ma1, standardize=TRUE)
plot(scale(res.ma1), main = "Residual plot, MA(1) model", ylab = "Standardized Residuals")


