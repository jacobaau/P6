library(tseries)
BTChist <- read.csv("P6BTC.csv")

price <- data.frame(BTChist$Close)
plot(price)

price.w <- as.numeric(price[seq(1, nrow(price), 7), ])
plot(price.w)

price.w.ts <- ts(price.w)
plot(price.w.ts)

return.w1 <- rep(0, 343)
for (t in 2: 343) {
  return.w1[t] <- (price.w[t]-price.w[t-1])/price.w[t-1]
}
return.w <- return.w1[-1]

plot(ts(return.w))

acf(ts(return.w), lag.max=342)

adf.test(ts(return.w))

# LOG RETURNS
return.w.log1 <- rep(0, 343)
for (t in 2: 343) {
  return.w.log1[t] <- log(price.w[t])-log(price.w[t-1])
}
return.w.log <- return.w.log1[-1]

plot(return.w.log)

plot(ts(return.w.log))

acf(ts(return.w.log), lag.max=342)

adf.test(ts(return.w.log)) 

#fitted AR modeller
ar1 <- arima(ts(return.w), order=c(1,0,0))
ar1.fit <- return.w-residuals(ar1)


plot(ts(return.w))
lines(ar1.fit, lty=2, col="red", lwd=2)

plot(ar1.fit)


par(mfrow=c(1,1))
acf(residuals(ar1), lag.max=342)

AIC(ar1)
BIC(ar1)

ar2 <- arima(ts(return.w), order=c(2,0,0))
ar2.fit <- return.w-residuals(ar2)


plot(ar2.fit)
plot(ts(return.w))

par(mfrow=c(1,1))
acf(residuals(ar2), lag.max=342)

AIC(ar2)
BIC(ar2) # It is seen AIC and BIC increases, implying AR1 model is the best to the data.

n <- 342
x <- ts(return.w) 
x1 <- c(NA, return.w)  # Lag(x,1) med NA som f?rste v?rdi

z <- x[-1]
z1 <- x1[c(-1, -343)]

model <- lm(z ~ z1)
res <- residuals(model)
summary(model) #p-v?rdi lig 0.38815, 0 hypotese phi=0 accepteret, emh opfyldt, returns=WN
plot(model)

#FOR LOG RETURNS
ar1 <- arima(ts(return.w.log), order=c(1,0,0))
ar1.fit <- return.w.log-residuals(ar1)

par(mfrow=c(1,2))
plot(ar1.fit)
plot(ts(return.w.log))

par(mfrow=c(1,1))
acf(residuals(ar1), lag.max=342)

AIC(ar1)
BIC(ar1)

ar2 <- arima(ts(return.w.log), order=c(2,0,0))
ar2.fit <- return.w.log-residuals(ar2)


plot(ar2.fit)
plot(ts(return.w.log))

par(mfrow=c(1,1))
acf(residuals(ar2), lag.max=342)

AIC(ar2)
BIC(ar2) # It is seen AIC and BIC increases, implying AR1 model is the best to the data.

n <- 342
x <- ts(return.w.log) 
x1 <- c(NA, return.w.log)  # Lag(x,1) med NA som f?rste v?rdi

z <- x[-1]
z1 <- x1[c(-1, -343)]

model <- lm(z ~ z1)
res <- residuals(model)
summary(model) #p-v?rdi lig 0.6368  , 0 hypotese phi=0 accepteret, emh opfyldt, returns=WN
plot(model)

