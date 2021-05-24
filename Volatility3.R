library(tseries)
library(astsa)
BTC <- read.csv("P6BTC.csv")

price <- data.frame(BTC$Close)

price.d <- data.frame(price[seq(1933, 2401),])

colnames(price.d) <- "DailyPrices"

price.d <- price.d[!price.d$DailyPrices=="null",]


price.d <- ts(as.numeric(price.d))

log.price.d <- log(price.d)

log.return <- ts(diff(log.price.d)[-71])

sq.log.return <- log.return^2

plot(sq.log.return, ylab = "Squared log-returns")

acf2(sq.log.return, max.lag = 100, main ="Correlogram of squared log-returns")

adf.test(sq.log.return)
adf.test(sq.log.return, k=0)

#Model 1: arch1
#Model 2: arch2
#Model 3: garch11
#Model 4: garch21
#Model 5: garch12
#Model 6: garch22
#Model 8: garch31


# garch32, garch23, garch33 producing NAs

library(fGarch)
arch1 <- garchFit(~garch(6,0), data = log.return, trace= FALSE)
arch1 # Variance is significant, alpha1 insignificant
#mean af log.return significant as expected
aic1 <- arch1@fit$ics[1]
bic1 <- arch1@fit$ics[2]

arch2 <- garchFit(~garch(2,0), data = log.return, trace= FALSE)
arch2 # Variance is significant, alpha1 and alpha2 insignificant 
#mean af log.return significant as expected
aic2 <- arch2@fit$ics[1]
bic2 <- arch2@fit$ics[2]


garch11 <- garchFit(~garch(1,1), data = log.return, trace= FALSE)
garch11 # Everything is significant
aic3 <- garch11@fit$ics[1]
bic3 <- garch11@fit$ics[2]

garch21 <- garchFit(~garch(2,1), data = log.return, trace= FALSE)
garch21 # alpha1, alpha2 insignificant
aic4 <- garch21@fit$ics[1]
bic4 <- garch21@fit$ics[2]

garch12 <- garchFit(~garch(1,2), data = log.return, trace= FALSE)
garch12 # Beta1, beta2 insignificant
aic5 <- garch12@fit$ics[1]
bic5 <- garch12@fit$ics[2]

garch22 <- garchFit(~garch(2,2), data = log.return, trace= FALSE)
garch22 # everything, except for mu and omega, is significant
aic6 <- garch22@fit$ics[1]
bic6 <- garch22@fit$ics[2]

garch31 <- garchFit(~garch(3,1), data = log.return, trace= FALSE)
garch31 #alpha1, alpha2, alpha3 insignificant
aic7 <- garch31@fit$ics[1]
bic7 <- garch31@fit$ics[2]
#
#
#
#
#
#

#
#
#
#
#
#

garch13 <- garchFit(~garch(1,3), data = log.return, trace= FALSE)
garch13 #producing NAs

garch32 <- garchFit(~garch(3,2), data = log.return, trace= FALSE)
garch32 #Producing NAs


garch23 <- garchFit(~garch(2,3), data = log.return, trace= FALSE)
garch23 #Producing NAs

garch33 <- garchFit(~garch(3,3), data = log.return, trace= FALSE)
garch33 #Producing NAs


aic <- c(aic1, aic2, aic3, aic4, aic5, aic6, aic7)
bic <- c(bic1, bic2, bic3, bic4, bic5, bic6, bic7)

plot(aic, type="b")
plot(bic, type="b")

plot(aic, type="b", ylim = c(-3.92, -3.8), ylab = "Information Criteria", 
     xlab = "Model", col = "red", lwd=2)
points(bic, type="b", col= "blue", lwd =2)
legend(6.1, -3.80, legend = c("BIC", "AIC"), col=c("blue", "red"),
       pch = c(1,1), pt.lwd = c(2,2))

which.min(aic)
which.min(bic)


aic[3] 
bic[3]
#garch11 is best according to bic

aic[7]
bic[7]
#garch31 is best according to aic


#MSE
mean(residuals(arch1)^2)
mean(residuals(arch2)^2)
mean(residuals(garch11)^2)
mean(residuals(garch21)^2)
mean(residuals(garch12)^2)
mean(residuals(garch22)^2)
mean(residuals(garch31)^2)

#garch11 is better than garch31 according to MSE

dev.mean_BitCoinReturn <- log.return

plot(dev.mean_BitCoinReturn, col="black",
     ylab = "log-return", 
     xlab = "Day",
     main = "Fitted GARCH(1,1) model to the Volatility of log-returns")


#add ARCH(1) confidence bands (one standard deviation) to the plot
lines(garch11@fit$coef[1] + garch11@sigma.t, col = "red")
lines(garch11@fit$coef[1] - garch11@sigma.t, col = "red")

lines(garch31@fit$coef[1] + garch31@sigma.t, col = "blue")
lines(garch31@fit$coef[1] - garch31@sigma.t, col = "blue")

lines(arch1@fit$coef[1] + arch1@sigma.t, col = "darkgreen")
lines(arch1@fit$coef[1] - arch1@sigma.t, col = "darkgreen")

abline(h=mean(log.return), col="blue")


abline(h= mean(log.return) + sd(log.return), col="darkgreen")
abline(h= mean(log.return) - sd(log.return), col="darkgreen")

ma1.garch11 <- garchFit(~arma(0,1)+garch(1,1), data = log.return, trace= FALSE)

ma1.garch11
garch11
