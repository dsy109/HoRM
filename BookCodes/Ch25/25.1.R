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
### Example 25.1.1: Radon Data
###############################################################################

data(radon, package = "ggmcmc")
radon <- HLMdiag::radon
names(radon)[2] <- "floor"

# Varying Intercept, No Predictor
M0 <- lmer(log.radon ~ 1 + (1 | county), data = radon)
summary(M0)
fixef(M0)
ranef(M0)

df <- data.frame(x = 1:85, y = fixef(M0) + unlist(ranef(M0)))
ggplot(data = df, aes(x, y)) + geom_point(size = 3) + ylab("Intercept") + xlab("County") + 
    geom_hline(yintercept = fixef(M0)) + xlim(levels(radon$county.name)) + theme(text = element_text(size = 13), 
    axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Intercepts: Varying Intercepts, No Predictor Model")

# Varying Intercept, Individual-Level Predictor
M1 <- lmer(log.radon ~ floor + (1 | county), data = radon)
summary(M1)
fixef(M1)
ranef(M1)

# Varying Slope
M2 <- lmer(log.radon ~ floor + (0 + floor | county), data = radon)
summary(M2)
fixef(M2)
ranef(M2)

# Varying Intercept, Varying Slope
M3 <- lmer(log.radon ~ floor + (1 + floor | county), data = radon)
summary(M3)
fixef(M3)
ranef(M3)

ind <- c(2, 4, 14, 19, 26, 37, 70, 80, 83)
radon.sub <- radon[radon$county %in% ind, ]
set.seed(100)
radon.sub$floor <- radon.sub$floor + runif(nrow(radon.sub), -0.01, 0.01)
radon.fits <- data.frame(county.name = unique(radon.sub$county.name), VI.int = fixef(M1)[1] + 
    unlist(ranef(M1))[ind], VI.slope = fixef(M1)[2], VS.int = fixef(M2)[1], VS.slope = fixef(M2)[2] + 
    unlist(ranef(M2))[ind], VIVS.int = fixef(M3)[1] + unlist(ranef(M3))[ind], VIVS.slope = fixef(M3)[2] + 
    unlist(ranef(M3))[ind])
ggplot(radon.sub, aes(x = floor, y = log.radon)) + geom_point() + facet_wrap(~county.name, 
    ncol = 3) + xlab("Floor") + ylab("Log(Radon)") + geom_abline(data = radon.fits, 
    aes(slope = VI.slope, intercept = VI.int), size = 1.2) + geom_abline(data = radon.fits, 
    aes(slope = VS.slope, intercept = VS.int), size = 1.2, col = 2, linetype = 2) + 
    geom_abline(data = radon.fits, aes(slope = VIVS.slope, intercept = VIVS.int), 
        size = 1.2, col = 3, linetype = 3) + theme(text = element_text(size = 15))

