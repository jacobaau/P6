library(tseries)
library(astsa)
BTC <- read.csv("P6BTC2.csv")

price <- data.frame(BTC$Close)

price.w <- ts(as.numeric(price[seq(1, nrow(price), 7), ]))

sum(is.na(price.w))
price.w <- price.w[-125]
price.w <- price.w[-149]

log.price.w <- log(price.w)

plot(price.w, main = "Weekly Bitcoin prices, 2017-12-01 to 2021-04-13", 
     xlab = "Week", ylab = "Dollars", lwd=1.5)
plot(log.price.w, main = "Weekly Bitcoin log-prices, 2017-12-01 to 2021-04-13", 
     xlab = "Week", ylab = "log(Dollars)", lwd=1.5)

adf.test(price.w, k=0)
adf.test(log.price.w)
log.return <- diff(log.price.w)

ar0.0 <- lm(log.return ~ 1)

res <- residuals(ar0.0, standardize = TRUE)

plot(res, main = "Residual plot of AR(0) model")

acf(ts(log.return), main = "Weekly Bitcoin log-return, 2017-12-01 to 2021-04-13")
pacf(ts(log.return), main = "Weekly Bitcoin log-return, 2017-12-01 to 2021-04-13")
adf.test(log.return)
adf.test(log.return, k=0)
arma01 <- arima(log.return, order=c(0,0,1))

arma11 <- arima(log.return, order=c(1,0,1))

fit.ar10 <- log.return - residuals(ar10)

fit.ar1 <- log.return[-1] - residuals(ar1)

fit.mean <- log.return - residuals(ar0)

arma10 <- arima(log.return, order=c(10,0,0))

fit.arma01 <- log.return - residuals(arma01)

arma11 <- arima(log.return, order=c(1,0,1))

fit.arma11 <- log.return - residuals(arma11)

arma12 <- arima(log.return, order=c(1,0,2))

fit.arma12 <- log.return - residuals(arma12)

arma22 <- arima(log.return, order=c(2,0,2))

fit.arma22 <- log.return - residuals(arma22)

AIC.vec <- c(AIC(arma01), AIC(arma11), AIC(arma10), AIC(arma11), AIC(arma12), AIC(arma22))
BIC.vec <- c(BIC(arma01), BIC(arma11), BIC(arma10), BIC(arma11), BIC(arma12), BIC(arma22))
plot(AIC.vec, type="b", lwd=2, ylim=c(min(AIC.vec), max(BIC.vec)), main="AIC and BIC for Information Set 2", ylab="Information Criteria")
points(BIC.vec, type="b", col="red", lwd=2)
legend(5,-520, legend=c("AIC", "BIC"), col=c("black", "red"), pch=c(1,1), pt.lwd=c(2,2))

lm(log.return[-1] ~ log.return[-170]) # Intercept is 0.007273 which means one has an average of 0.7% log-return each week.

arma00 <- arima(log.return, order=c(0,0,0))
mean.model <- lm(log.return ~ 1)
summary(mean.model)
