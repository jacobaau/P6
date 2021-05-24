library(tseries)
library(astsa)
BTC <- read.csv("P6BTC2.csv")

price <- data.frame(BTC$Close)

price.w <- ts(as.numeric(price[seq(1, nrow(price), 7), ]))
price.w <- price.w[-125]
price.w <- price.w[-149]


price.w <- ts(as.numeric(price.w))

log.price.w <- log(price.w)

log.return <- diff(log.price.w)

sq.log.return <- log.return^2

plot(sq.log.return, ylab="Squared log-return")
#Plot denne, mange volatility cluster
acf2(sq.log.return, max.lag = 100)

adf.test(sq.log.return)
adf.test(sq.log.return, k=0)
#station?rt efter df-0 og adf test
# siden at der ikke noget korrelation vil man tro det er hvid st?j

#Model 1: arch1
#Model 2: arch2
#Model 3: garch11
#Model 4: garch21
#Model 5: garch22
#Model 6: garch31
#Model 7: garch13
#Model 8: garch32
#Model 9: garch33

library(fGarch)
arch1 <- garchFit(~garch(1,0), data = log.return, trace= FALSE)
arch1
# fra conditional distribution, varians af log-return er signifikant (omega)
aic1 <- arch1@fit$ics[1]
bic1 <- arch1@fit$ics[2]

arch2 <- garchFit(~garch(2,0), data = log.return, trace= FALSE)
arch2
#omega er signifikant (varians af log-return) alpha2 er t?t p?
aic2 <- arch2@fit$ics[1]
bic2 <- arch2@fit$ics[2]

garch11 <- garchFit(~garch(1,1), data = log.return, trace= FALSE)
garch11
#beta1 koefficient til f?rste lag af st?j er signifikant omega er t?t p?
aic3 <- garch11@fit$ics[1]
bic3 <- garch11@fit$ics[2]

garch21 <- garchFit(~garch(2,1), data = log.return, trace= FALSE)
garch21
aic4 <- garch21@fit$ics[1]
bic4 <- garch21@fit$ics[2]

garch12 <- garchFit(~garch(1,2), data = log.return, trace= FALSE)
garch12 #Producing NAs, forkastet

garch22 <- garchFit(~garch(2,2), data = log.return, trace= FALSE)
garch22 # forkastes

garch13 <- garchFit(~garch(3,1), data = log.return, trace= FALSE)
garch13 # forkastes

garch31 <- garchFit(~garch(3,1), data = log.return, trace= FALSE)
garch31 # forkastes

garch32 <- garchFit(~garch(3,2), data = log.return, trace= FALSE)
garch32 # forkastes

garch23 <- garchFit(~garch(2,3), data = log.return, trace= FALSE)
garch23 #Producing NAs, forkastes

garch33 <- garchFit(~garch(3,3), data = log.return, trace= FALSE)
garch33 # forkastes

aic <- c(aic1, aic2, aic3, aic4)
bic <- c(bic1, bic2, bic3, bic4)

plot(aic, type="b", lwd=2, ylim=c(min(aic), max(bic)), 
     main="AIC and BIC for Information Set 2", ylab="Information Criteria")
points(bic, type="b", col="red", lwd=2)
legend(3.5,-1.48, legend=c("AIC", "BIC"), col=c("black", "red"), 
       pch=c(1,1), pt.lwd=c(2,2))


which.min(aic)
which.min(bic)
#arch2 er min

#MSE
mean(residuals(arch1)^2)
mean(residuals(arch2)^2)
mean(residuals(garch11)^2)
mean(residuals(garch21)^2)
#arch1 har den mindste MSE

variance <- lm(sq.log.return ~1)
summary(variance)
#variansen er signifikant

arma01 <- arima(log.return, order=c(0,0,1))
fit.arma01 <- log.return - residuals(arma01)
AIC(variance)
BIC(variance)
#AIC og BIC er LANGT mindre end ved arch og garch modeller, varians er LANGT bedre

mean(residuals(variance)^2)
#LANGT lavere MSE, vi havde ogs? at mean af logreturn ikke er signifikant som korresponderer med det fra f?r

plot(log.return, col="black",
     ylab = "log-return", 
     xlab = "Week",
     main = "Fitted ARCH(2) and MA(1) model to the log-returns", 
     lwd=1.2)
points(fit.arma01, type="l", col="blue")

#add ARCH(2) confidence bands (one standard deviation) to the plot
lines(arch2@sigma.t, col = "red")
lines(-arch2@sigma.t, col = "red")


plot(scale(residuals(arch2)))
qqnorm(scale(residuals(arch2)))
qqline(scale(residuals(arch2)))

#variansen har bedre AIC, BIC, MSE. ARCH(2) ser p?nere ud p? grafen,
# folk kan diskutere hvorvidt hvilken en er bedst. den r?de er fleksibel. den r?de varierer ikke S? meget
# igen, til at man kan f? noget ud af det
# in conclusion, if one would only follow the criteria, the variance is best
# from these criteria the volatility can not be modelled