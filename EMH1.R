library(tseries)
library(astsa)
BTC <- read.csv("P6BTC.csv")

price <- data.frame(BTC$Close)

price.w <- ts(as.numeric(price[seq(1, nrow(price), 7), ]))

sum(is.na(price.w))

log.price.w <- log(price.w)

plot(price.w, main = "Weekly Bitcoin prices, 2014-09-17 to 2021-04-07", 
     xlab = "Week", ylab = "Dollars", lwd=1.5)
plot(log.price.w, main = "Weekly Bitcoin log-prices, 2014-09-17 to 2021-04-07", 
     xlab = "Week", ylab = "log(Dollars)", lwd=1.5)
adf.test(price.w, k=0)
adf.test(log.price.w)
log.return <- diff(log.price.w)

hw <- HoltWinters(log.return, beta=FALSE, gamma=FALSE)

arima010 <- arima(log.price.w, order = c(10,1,9))

fit.arima010 <- log.price.w - residuals(hw)

points(fit.arima010, type="l", col="red")

plot(log.return, main = "Weekly log-returns, 2014-09-17 to 2021-04-07", 
     xlab = "Week", ylab = "log-return")
abline(h=0)

m <- mean(log.return)
s <- sd(log.return)
hist(log.return, prob=TRUE, breaks=100, col="light green",
     main = "Histogram of log-returns", xlab = "log-return")
curve(dnorm(x, mean = m, sd = s), add = TRUE, lwd = 2, col = "blue")
min(log.return)
max(log.return)

acf(log.return, main = "Correlogram of log-return")
pacf(log.return, main = "Correlogram of log-return", 
     lag.max = length(log.return))

acf2(log.return, main = "Correlogram of log-return", lwd=2, 
     max.lag = length(log.return)-1)

#min lag 83 
#max lag 19 (10)

acf2(log.return, main = "Correlogram of log-return", lwd=2)

#min lag 44 (42) max lag 19 (10)

adf.test(log.return) #ADF test
adf.test(log.return, k=0) #DF-0 test

ar1 <- arima(log.return, order=c(1,0,0))
ar1

#regress on lag 10

ar1 <- lm(log.return[-1]~log.return[-342])
ar1 <- arima(log.return, order = c(1,0,0))
summary(ar1.1)
summary(ar1)


f10 <- seq(1, 10)
b10 <- seq(333, 342)

f9 <- seq(1, 9)
b9 <- seq(334, 342)

f9 <- seq(1, 9)
b9 <- seq(334, 342)

f42 <- seq(1, 42)
b42 <- seq(301, 342)
b52 <- seq(301, 342)

ar10 <- lm(log.return[-f10]~log.return[-b10])

ar42 <- lm(log.return[-f42]~log.return[-b42])

ar10.42 <- lm(log.return[-f42]~ log.return[-b42])

#regress on lag 19

p0 <- log.price.w[1]

pt <- rep(0, 342)

p <- c(p0, pt)

for(t in 2:343){
  p[t] <- exp(0.014)*p[t-1]
}

ar0.0 <- lm(log.return ~ 1)
summary(ar0.0)

ar0 <- arima(log.return, order=c(0,0,0))
res <- residuals(ar0.0)

plot(res, main = "Residual plot of AR(0) model")



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

plot(ts(log.return), main = "Weekly log-returns and the fitted models", 
     xlab = "Week", ylab = "log-return")
abline(h=mean(log.return))

points(fit.ar1,type="b"  ,col = "blue", lwd =2)

points(fit.arma01,type="b"  ,col = "blue", lwd =2)

points(fit.arma02,type="b"  ,col = "blue", lwd =2)

points(fit.arma12,type ="l"  ,col = "red", lwd =2)

points(fit.arma11,type = "b" ,col = "darkgreen", lwd =2)

points(fit.arma11,type = "b" ,col = "darkgreen", lwd =2)

legend(290, -0.3, legend = c("BIC", "AIC"),
       col = c("black", "blue"), pch = c(1,1), pt.lwd = c(2,2))

resids <- residuals(ar0.0, standardize = TRUE)
plot(resids, main = "Residual Plot", ylab = "Standardized Residual")
abline(h=0)
#It looks like a randomize cloud, but there seems to be some
# sign of heteroschedascticity. However not that bad.

acf(resids, lag.max = length(log.return), main = "ACF of The Residuals")

m <- mean(resids)
s <- sd(resids)
hist(resids, prob=TRUE, breaks=100, col="light green",
     main = "Histogram of the Residuals", xlab = "Residuals")
curve(dnorm(x, mean = m, sd = s), add = TRUE, lwd = 1.5, col = "blue")
qqnorm(resids, main = "Normal Q-Q Plot of the Residuals")
qqline(resids, col = "red", lwd = 1.5)

