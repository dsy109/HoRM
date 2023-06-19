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
### Example 25.14.1: Belgium Telephone Data
###############################################################################

data(telephone, package = "Rfit")
fit <- rfit(calls ~ year, data = telephone)
summary(fit)

lm <- lm(calls ~ year, data = telephone)
df16 <- data.frame(x = telephone$year, y = telephone$calls)

ggplot(data = df16, aes(x, y)) + geom_point() + geom_abline(slope = fit$coefficients[2], 
    intercept = fit$coefficients[1]) + geom_abline(slope = lm$coefficients[2], intercept = lm$coefficients[1], 
    col = 2, linetype = 2) + labs(title = "Belgium Telephone Data", y = "Calls (Tens of Millions)", 
    x = "Year") + theme(text = element_text(size = 20))

