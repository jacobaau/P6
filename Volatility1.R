# Modeling the volatility of BitCoin-returns

library(tseries)

BTC <- read.csv("P6BTC.csv")

price <- data.frame(BTC$Close)

dates <- data.frame(BTC$Date)

price.w <- ts(as.numeric(price[seq(1, nrow(price), 7), ]))

dates.w <- as.Date(dates[seq(1, nrow(price), 7), ])

sum(is.na(price.w))

plot(log(price.w))

log.return <- diff(log(price.w))

n <- length(log.return)

plot(log.return)

acf(log.return)
acf(log.return, lag.max = n)
#Looks like white noise

adf.test(log.return)
adf.test(log.return, k=0)
#stationary for all lags


sq.log.return <- log.return^2
abs.log.return <- abs(log.return)

plot(sq.log.return, main = "Squared Return", col="blue")
points(arch@sigma.t^2, type="l", col="red")
plot(abs.log.return, main = "Absolute Return")
#There is definitely volatility clutering

par(mfrow=c(1,2))
acf(sq.log.return)
par(mfrow=c(1,2))
acf(sq.log.return, lag.max = 50, main = "ACF of Squared Return")
pacf(sq.log.return, lag.max = 50, main = "PACF of Squared Return")
#acf2(sq.log.return, max.lag = n-1,
#     main = "Sample ACF and PACF of Squared Return")
#Several breach the confidens interval, but there is some correlation that
# are significant, specially at lag 1 and one between 250 and 300 lags.
acf(abs.log.return)
acf(abs.log.return, lag.max = n)
#However in the ACF of abs it only seems that the first lags is fignificant.
adf.test(sq.log.return, k=0)
#It is believe thatthe squared and abs return both are stationary processes

sq.log.return <- (log.return-mean(log.return))^2

variance <- lm(sq.log.return ~ 1)
AIC(variance)
BIC(variance)
mean(residuals(variance)^2)

plot(scale(residuals(arch)))

plot(scale(residuals(variance)))

summary(variance)

#Fits a ARCH to the log-return
library(fGarch)
arch <- garchFit(~garch(1,0), data = log.return, trace= FALSE)
arch
summary(arch)
# r_t = mu + sigma_t epsilon_t 
# sigma_t^2 = omega + alpha_1 r_{t-1}^2 + v_t
# mu is significant -> interesting
# omega and alpha_1 is significant

# the standardised residuals is (estimators of v_t)
resids <- residuals(arch, standardize = TRUE)
plot(resids, main = "Residual Plot", ylab = "Standardized Residual")
abline(h=0)
#It looks like a randomize cloud, but there seems to be some
# sign of heteroschedascticity. However not that bad.

acf(resids, lag.max = n-1, main = "ACF of The Residuals")

m <- mean(resids)
s <- sd(resids)
hist(resids, prob=TRUE, breaks=100, col="light green",
     main = "Histogram of the Residuals", xlab = "Residuals")
curve(dnorm(x, mean = m, sd = s), add = TRUE, lwd = 1.5, col = "blue")
qqnorm(resids, main = "Normal Q-Q Plot of the Residuals")
qqline(resids, col = "red", lwd = 1.5)
# There is heavy tails, leptokurtic distribution of the noise

# compute deviations of the percentage changes from their mean
dev.mean_BitCoinReturn <- log.return

plot(dev.mean_BitCoinReturn, col="black",
     ylab = "log-return", 
     xlab = "Week",
     main = "Fitted ARCH(1) model to the Volatility of log-returns")
abline(h=mean(log.return), col="blue")

#add ARCH(1) confidence bands (one standard deviation) to the plot
lines(arch@fit$coef[1] + arch@sigma.t, col = "red")
lines(arch@fit$coef[1] - arch@sigma.t, col = "red")
# The ARCH(1)-model seems to catch the the volatility clusters pretty good
# It was explored that the the second coefficients of the ARCH(2), and ARCH(3) are not
# not signififcant



#Investigating the BIC of fitted ARCH(p) for varying order

BICs <- rep(0,10)
AICs <- rep(0,10)
arch1 <- garchFit(~garch(1,0), data = log.return, trace= FALSE)
AICs[1] <- arch1@fit$ics[1]
BICs[1] <- arch1@fit$ics[2]

arch2 <- garchFit(~garch(1,1), data = log.return, trace= FALSE)
AICs[2] <- arch2@fit$ics[1]
BICs[2] <- arch2@fit$ics[2]

arch3 <- garchFit(~garch(1,2), data = log.return, trace= FALSE)
AICs[3] <- arch3@fit$ics[1]
BICs[3] <- arch3@fit$ics[2]

arch4 <- garchFit(~garch(1,3), data = log.return, trace= FALSE)
AICs[4] <- arch4@fit$ics[1]
BICs[4] <- arch4@fit$ics[2]

arch5 <- garchFit(~garch(1,4), data = log.return, trace= FALSE)
AICs[5] <- arch5@fit$ics[1]
BICs[5] <- arch5@fit$ics[2]

arch6 <- garchFit(~garch(1,5), data = log.return, trace= FALSE)
AICs[6] <- arch6@fit$ics[1]
BICs[6] <- arch6@fit$ics[2]

arch7 <- garchFit(~garch(1,6), data = log.return, trace= FALSE)
AICs[7] <- arch7@fit$ics[1]
BICs[7] <- arch7@fit$ics[2]

arch8 <- garchFit(~garch(1,7), data = log.return, trace= FALSE)
AICs[8] <- arch8@fit$ics[1]
BICs[8] <- arch8@fit$ics[2]

arch9 <- garchFit(~garch(1,8), data = log.return, trace= FALSE)
AICs[9] <- arch9@fit$ics[1]
BICs[9] <- arch9@fit$ics[2]

arch10 <- garchFit(~garch(1,9), data = log.return, trace= FALSE)
AICs[10] <- arch10@fit$ics[1]
BICs[10] <- arch10@fit$ics[2]

plot(BICs)
plot(AICs)
plot(BICs, type = "b", ylim = c(-1.75, -1.61), ylab = "Information Criteria", 
     main = "Informations Criteria for ARCH(p) models", lwd=2, xlab = "Order")
points(AICs, type = "b", col="blue", lwd=2)
legend(9, -1.66, legend = c("BIC", "AIC"),
       col = c("black", "blue"), pch = c(1,1), pt.lwd = c(2,2))
arch10

#Fits a GARCH(1,1) model, where varepsilon is normal
garch <- garchFit(~garch(1,2), data = log.return, trace= FALSE)
garch
# The coefficient to the first lag of the previous volatility is not significant
resids.g <- residuals(garch, standardize = TRUE)
plot(resids.g)

m <- mean(resids.g)
s <- sd(resids.g)
hist(resids.g, prob=TRUE, breaks=200, col="light green")
curve(dnorm(x, mean = m, sd = s), add = TRUE, lwd = 1.5, col = "blue")
qqnorm(resids.g); qqline(resids.g, col = "red", lwd = 1.5)

dev.mean_BitCoinReturn.g <- log.return - garch@fit$coef[1]
plot(dev.mean_BitCoinReturn.g, 
     ype = "l", 
     col = "steelblue",
     ylab = "Percent", 
     xlab = "Date",
     main = "Estimated Bands of +- One Conditional Standard Deviation",
     lwd = 0.2)
abline(h=0)
lines(garch@fit$coef[1] + garch@sigma.t, col = "darkred", 
      lwd = 0.5)
lines(garch@fit$coef[1] - garch@sigma.t, col = "darkred", 
      lwd = 0.5)

# Fits a APARCH, where the epsilon is normal
aparch <- garchFit(~aparch(1,0), data = log.return, trace= FALSE)
#

aparch
aparch.F <- garchFit(~aparch(1,0), data = log.return, trace= FALSE, 
                   include.delta = FALSE)

aparch.F
#It does not seem that a APARCH is good for decribing volatility
