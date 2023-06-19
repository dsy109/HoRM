library(ggplot2)
library(HoRM)
library(quantmod)
library(astsa)
library(orcutt)
library(car)
library(forecast)
library(nlme)
library(HarmonicRegression)
library(prais)
library(lmtest)

###############################################################################
### Example 13.5.3: Natural Gas Prices Data
###############################################################################

data(gas, package = "HoRM")
attach(gas)

out <- lm(OK ~ LA, data = gas)
res <- ts(out$residuals, start = c(1988, 1), frequency = 12)

autoplot(ts(LA, start = c(1988, 1), frequency = 12), main = "Louisiana Gas Prices") + 
    labs(y = "Prices") + theme(text = element_text(size = 20))
autoplot(ts(OK, start = c(1988, 1), frequency = 12), main = "Oklahoma Gas Prices") + 
    labs(y = "Prices") + theme(text = element_text(size = 20))

ggAcf(res, main = "Natural Gas Prices") + theme(text = element_text(size = 20))
ggPacf(res, main = "Natural Gas Prices") + theme(text = element_text(size = 20))

durbinWatsonTest(out, max.lag = 1)

reg1 <- cochrane.orcutt(out)
reg1
reg1a <- prais.winsten(OK ~ LA, data = gas)
reg1a

durbinWatsonTest(reg1[[1]]$residuals, max.lag = 2)
durbinWatsonTest(reg1a[[1]]$residuals, max.lag = 2)

rho <- seq(0.1, 0.99, by = 0.01)
hl.out <- data.frame(rho = rho, SSE = sapply(1:length(rho), function(i) deviance(hildreth.lu(y = OK, 
    x = LA, rho = rho[i]))))
rho.hat <- rho[which.min(hl.out[, 2])]
qplot(rho, SSE, data = hl.out, geom = "line") + theme(text = element_text(size = 20)) + 
    xlab(expression(rho)) + ylab("SSE")
reg2 <- hildreth.lu(rho.hat, y = OK, x = LA)
summary(reg2)
durbinWatsonTest(reg2, max.lag = 2)

reg3 <- lm(diff(OK) ~ 0 + diff(LA))
summary(reg3)
durbinWatsonTest(reg3, max.lag = 2)

