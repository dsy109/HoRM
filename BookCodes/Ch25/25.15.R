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
### Example 25.15.1: Ethanol Fuel Data
###############################################################################

data(NOdata, package = "mixtools")
set.seed(100)
out.em <- regmixEM(NOdata$Equivalence, NOdata$NO, k = 2)
mix.lab <- apply(out.em$posterior, 1, which.max) + 1
out.em[3:6]

df17 <- data.frame(x = NOdata$NO, y = NOdata$Equivalence, group = mix.lab)
ggplot(data = df17, aes(x, y)) + geom_point() + labs(title = "Ethanol Fuel Data", 
    x = "NO", y = "Equivalence") + theme(text = element_text(size = 20))

df17$group <- as.factor(df17$group)
ggplot(data = df17, aes(x, y, col = group, pch = group)) + geom_point() + labs(title = "Ethanol Fuel Data", 
    x = "NO", y = "Equivalence") + geom_abline(slope = out.em$beta[, 1][2], intercept = out.em$beta[, 
    1][1], col = 2) + geom_abline(slope = out.em$beta[, 2][2], intercept = out.em$beta[, 
    2][1], col = 3) + scale_color_manual("", values = c(2, 3), guide = F) + theme(text = element_text(size = 20), 
    legend.position = "none")

