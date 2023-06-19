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
### Example 13.5.2: Cardiovascular Data
###############################################################################

data(lap, package = "astsa")

out <- lm(cmort ~ tempr + part, data = lap)
summary(out)
lap.res <- zoo(out$residuals, index(lap))
autoplot(lap[, 1], main = "Cardiovascular Data") + labs(y = "Cardiovascular Mortality") + 
    theme(text = element_text(size = 20))
autoplot(lap[, 4], main = "Cardiovascular Data") + labs(y = "Temperature") + theme(text = element_text(size = 20))
autoplot(lap[, 11], main = "Cardiovascular Data") + labs(y = "Particulates") + theme(text = element_text(size = 20))
autoplot(lap.res, main = "Cardiovascular Data") + labs(y = "Raw Residuals") + theme(text = element_text(size = 20))
ggAcf(lap.res, main = "Cardiovascular Data") + theme(text = element_text(size = 20))
ggPacf(lap.res, main = "Cardiovascular Data") + theme(text = element_text(size = 20))

durbinWatsonTest(out)
bgtest(out)

out1 <- gls(cmort ~ tempr + part, data = lap, correlation = corAR1(), method = "ML")
out2 <- gls(cmort ~ tempr + part, data = lap, correlation = corARMA(p = 2), method = "ML")
anova(out1, out2)

out1W <- gls(cmort ~ tempr + part, data = lap, correlation = corAR1(), method = "ML", 
    weights = varPower())
out2W <- gls(cmort ~ tempr + part, data = lap, correlation = corARMA(p = 2), method = "ML", 
    weights = varPower())

