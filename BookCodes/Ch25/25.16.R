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
### Example 25.16.1: Cardiovascular Data (cont.)
###############################################################################

data(lap, package = "astsa")
lap2 <- data.frame(lap)
all.fits <- lapply(0:6, function(i) gcmr(cmort ~ tempr + part, data = lap2, marginal = gaussian.marg, 
    cormat = arma.cormat(1, i)))
all.aics <- sapply(1:7, function(i) summary(all.fits[[i]])$aic)
copula.fit <- all.fits[[which.min(all.aics)]]
summary(copula.fit)
copula.fit2 <- gcmr(cmort ~ part, data = lap2, marginal = gaussian.marg, cormat = arma.cormat(1, 
    3))
AIC(copula.fit, copula.fit2)

aic.df <- data.frame(AIC = all.aics, q = 0:6)
ggplot(aic.df, aes(x = q, y = AIC)) + geom_line() + geom_point(cex = 3) + geom_point(aes(x = 3, 
    y = all.aics[4]), col = "red", pch = 8, cex = 5) + theme(text = element_text(size = 20)) + 
    xlab("q") + ylab("AIC") + ggtitle("AICs for Different ARMA(1,q) Fits")

qres <- residuals(copula.fit)
fits <- copula.fit$fitted.values

res <- data.frame(res = qres, y = lap2$cmort, yhat = fits, obs = 1:508)

ggplot(res, aes(sample = qres)) + stat_qq() + geom_abline(intercept = 0, slope = 1, 
    color = 4) + theme(text = element_text(size = 20)) + xlab("Normal Quantiles") + 
    ylab("Sorted Quantile Residuals") + ggtitle("Normal Q-Q Plot")

ggplot(res, aes(x = obs, y = res)) + geom_line() + geom_hline(yintercept = 0) + theme(text = element_text(size = 20)) + 
    xlab("Observation Number") + ylab("Quantile Residuals") + ggtitle("Residuals vs. Indices of Observation")

ggplot(res, aes(x = y, y = yhat)) + geom_point() + geom_abline(intercept = 0, slope = 1) + 
    theme(text = element_text(size = 20)) + xlab("Observed Values") + ylab("Predicted Values") + 
    ggtitle("Predicted vs. Observed Values")

ggplot(res, aes(x = yhat, y = res)) + geom_point() + geom_hline(yintercept = 0) + 
    theme(text = element_text(size = 20)) + xlab("Predicted Values") + ylab("Quantile Residuals") + 
    ggtitle("Quantile Residuals vs. Predicted Values")

