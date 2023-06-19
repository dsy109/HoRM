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
### Example 25.13.1: Wind Direction Data
###############################################################################

data(wind, package = "NPCirc")
wind6 <- circular(wind$wind.dir[seq(7, 1752, by = 24)])
wind12 <- circular(wind$wind.dir[seq(13, 1752, by = 24)])
lm.out <- lm.circular(y = wind12, x = wind6, type = "c-c", order = 2)
lm.out[c(5:6, 8)]

df15 <- data.frame(x = as.numeric(wind6), y = as.numeric(wind12), lx = as.numeric(wind6[order(wind6)]), 
    ly = lm.out$fitted[order(wind6)])
ggplot(data = df15, aes(x, y)) + geom_point() + geom_line(data = df15, aes(lx, ly), 
    col = 2) + theme(text = element_text(size = 20)) + labs(title = "Circular-Circular Regression Fit", 
    x = "Wind Direction (6 a.m.)", y = "Wind Direction (12 p.m.)")

rose.diag(lm.out$residuals, bins = 18, main = "Residuals", tol = 0)
points(lm.out$residuals, stack = TRUE)

