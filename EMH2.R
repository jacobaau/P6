library(tseries)
library(astsa)
BTC <- read.csv("P6BTC2.csv")

price <- data.frame(BTC$Close)

price.w <- ts(as.numeric(price[seq(1, nrow(price), 7), ]))

sum(is.na(price.w))
price.w
price.w <- price.w[-125]
price.w <- price.w[-149]
price.w
log.price.w <- log(price.w)

plot(price.w, main = "Weekly Bitcoin prices, 2017-12-01 to 2021-04-13", 
     xlab = "Week", ylab = "Dollars", lwd=1.5)
plot(log.price.w, main = "Weekly Bitcoin log-prices, 2017-12-01 to 2021-04-13", 
     xlab = "Week", ylab = "log(Dollars)", lwd=1.5)

adf.test(price.w, k=0)
adf.test(log.price.w)
log.return <- diff(log.price.w)
plot(ts(log.return), ylab="Bitcoin log-return", xlab="Weeks")
ar0.0 <- lm(log.return ~ 1)

res <- residuals(ar0.0, standardize = TRUE)

plot(res, main = "Residual plot of AR(0) model")

acf(ts(log.return), main = "Weekly Bitcoin log-return, 2017-12-01 to 2021-04-13")
pacf(ts(log.return), main = "Weekly Bitcoin log-return, 2017-12-01 to 2021-04-13")
adf.test(log.return)
adf.test(log.return, k=0)

arma01 <- arima(log.return, order=c(0,0,1))
fit.arma01 <- log.return - residuals(arma01)
arma01

arma11 <- arima(log.return, order=c(1,0,1))

arma10 <- arima(log.return, order=c(1,0,0))
fit.arma10 <- log.return - residuals(arma10)

arma11 <- arima(log.return, order=c(1,0,1))

fit.arma11 <- log.return - residuals(arma11)

arma12 <- arima(log.return, order=c(1,0,2))

fit.arma12 <- log.return - residuals(arma12)

arma22 <- arima(log.return, order=c(2,0,2))

fit.arma22 <- log.return - residuals(arma22)

AIC.vec <- c(AIC(arma01), AIC(arma11), AIC(arma10), AIC(arma12), AIC(arma22))
BIC.vec <- c(BIC(arma01), BIC(arma11), BIC(arma10), BIC(arma12), BIC(arma22))
plot(AIC.vec, type="b", lwd=2, ylim=c(min(AIC.vec), max(BIC.vec)), main="AIC and BIC for Information Set 2", ylab="Information Criteria")
points(BIC.vec, type="b", col="red", lwd=2)
legend(4.5,-255, legend=c("AIC", "BIC"), col=c("black", "red"), pch=c(1,1), pt.lwd=c(2,2))

ar1 <- lm(log.return[-1] ~ log.return[-170])
summary(ar1)

arma00 <- arima(log.return, order=c(0,0,0))
mean.model <- lm(log.return ~ 1)
summary(mean.model)
mean0.model <- lm(log.return ~ -1)
summary(mean0.model)
AIC(mean0.model)
BIC(mean0.model)
mean(residuals(mean0.model)^2)

AIC(mean.model)
BIC(mean.model)
min(AIC.vec)
min(BIC.vec)
# AIC og BIC siger at mean model er bedst, ved AIC er det meget t?t p?, men ved BIC er der et lidt st?rre gap
mean(residuals(mean.model)^2)
mean(residuals(arma01)^2)
mean(residuals(arma10)^2)

#derfor kigger vi p? MSE, som antyder at arma01 er bedst

plot(ts(log.return), ylab="Bitcoin log-return", main="MA(1) Fitted to log-return, Information Set 2")
abline(h=0)
points(fit.arma01, type="l"  ,col = "red", lwd =2)


# Siden AR koefficienten ikke er signifikant, ser vi at EMH opfyldt, da mean modelen heller ikke er signifikant
# Den r?de og den bl? fit f?lger hinanden meget t?t