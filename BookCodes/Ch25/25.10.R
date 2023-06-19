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
### Example 25.10.1: Animal Weight Data (cont.)
###############################################################################

data(Animals, package = "MASS")
ir <- isoreg(log10(Animals$body), log10(Animals$brain))

df11 <- data.frame(x = log10(Animals$body), y = log10(Animals$brain), knot_x = sort(ir$x)[ir$iKnots], 
    knot_y = unique(ir$yf))

ggplot(data = df11, aes(x = x, y = y)) + geom_point(shape = 1) + geom_point(data = df11, 
    aes(x = knot_x, knot_y), col = 2, shape = 13, size = 2) + geom_step(data = df11, 
    aes(knot_x, knot_y), direction = "vh", col = 2) + ggtitle("Isotonic Regression") + 
    xlab(expression(paste("Body Weight (", log[10], " Scale)"))) + ylab(expression(paste("Brain Weight (", 
    log[10], " Scale)"))) + theme(text = element_text(size = 20))

df12 <- data.frame(x = sort(ir$x), y = cumsum(ir$y[order(ir$x)]), knot_x = sort(ir$x)[ir$iKnots], 
    knot_y = cumsum(ir$y[order(ir$x)])[ir$iKnots])
ggplot(data = df12, aes(x, y)) + geom_line(col = 2) + geom_point(data = df12, aes(x, 
    y), shape = 1) + geom_point(data = df12, aes(x = knot_x, y = knot_y), col = 2, 
    shape = 13, size = 2) + xlim(-2, 5) + xlab(expression(paste("Body Weight (", 
    log[10], " Scale)"))) + theme(text = element_text(size = 20)) + ylab("Cumulative Sum of Response") + 
    ggtitle("Cumulative Data and Convex Minorant")

