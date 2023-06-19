library(ggplot2)
library(MASS)
library(lattice)
library(HLMdiag)
library(fda)
library(rgl)
library(DepthProc)
library(alr3)
library(NPCirc)
library(quantreg)
library(mixtools)
library(Rfit)
library(extRemes)
library(ismev)
library(spdep)
library(multilevel)
library(survey)
library(SDaA)
library(bayesm)
library(coda)
library(ggmcmc)
library(SemiPar)
library(gcmr)
library(MAd)
library(metafor)
library(VIM)
library(astsa)
library(rTensor)
library(tensorA)
library(dplyr)
library(RandomFields)
library(lme4)

###############################################################################
### Example 25.12.1: Boston Housing Data (cont.)
###############################################################################

data(Boston, package = "MASS")
data(boston, package = "spdep")
Boston <- cbind(boston.c[, 1:5], Boston)
boston.lm <- lm(log(medv) ~ crim + zn + indus + chas + I(nox^2) + I(rm^2) + age + 
    log(dis) + log(rad) + tax + ptratio + black + log(lstat), data = Boston)
boston.w <- nb2listw(boston.soi)
boston.moran <- lm.morantest(boston.lm, boston.w, alternative = "two.sided")
boston.moran
boston.err <- errorsarlm(log(medv) ~ crim + zn + indus + chas + I(nox^2) + I(rm^2) + 
    age + log(dis) + log(rad) + tax + ptratio + black + log(lstat), data = Boston, 
    listw = boston.w)
summary(boston.err)

boston.df <- data.frame(Boston, Median.Value = exp(boston.err$fitted.values))
boston.df <- fortify(boston.df, region = TOWN)

boston.lon <- by(boston.df[, 4], boston.df[, "TOWN"], FUN = mean)
boston.lat <- by(boston.df[, 5], boston.df[, "TOWN"], FUN = mean)
boston.towns <- data.frame(LON = c(boston.lon), LAT = c(boston.lat), Median.Value = NA, 
    medv = NA)

ggplot(boston.df, aes(x = LON, y = LAT, z = medv)) + scale_fill_continuous(low = "white", 
    high = "black", name = "Median\nValue", limits = c(5, 60)) + geom_point(data = boston.towns, 
    aes(x = LON, y = LAT), cex = 1.5, col = "red") + stat_summary_hex(fun = mean, 
    bins = 15, alpha = 0.6) + theme(text = element_text(size = 20)) + xlab("Longitude") + 
    ylab("Latitude") + ggtitle("Boston Housing Data: Median House Price (Observed)")

ggplot(boston.df, aes(x = LON, y = LAT, z = Median.Value)) + scale_fill_continuous(low = "white", 
    high = "black", name = "Median\nValue", limits = c(5, 60)) + geom_point(data = boston.towns, 
    aes(x = LON, y = LAT), cex = 1.5, col = "red") + stat_summary_hex(fun = mean, 
    bins = 15, alpha = 0.6) + theme(text = element_text(size = 20)) + xlab("Longitude") + 
    ylab("Latitude") + ggtitle("Boston Housing Data: Median House Price (Predicted)")

