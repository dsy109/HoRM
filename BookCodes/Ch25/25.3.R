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
### Example 25.3.1: Animal Weight Data
###############################################################################

# Figure in Main Discussion
X <- 1:10
Y <- c(3, 6, 5.5, 5.75, 6.7, 11.2, 12.75, 9, 15, 11)
sim.df <- data.frame(X, Y, col = as.factor(c(1, 2, 1, 1, 1, 1, 1, 2, 1, 2)))
ggplot(sim.df, aes(x = X, y = Y, color = col, pch = col)) + geom_point(size = 3) + 
    geom_abline(slope = 1, intercept = 3, lwd = 1.2) + theme(text = element_text(size = 20), 
    legend.position = "none") + geom_hline(yintercept = 8.5, lty = 2, col = 4)

# Example
data(Animals, package = "MASS")
out.lm <- lm(log10(brain) ~ log10(body), data = Animals)
out.rd <- deepReg2d(log10(Animals$body), log10(Animals$brain))
out.lm
out.rd

df5 <- data.frame(Brain = log10(Animals$brain), Body = log10(Animals$body))
df6 <- data.frame(intercept = c(out.lm$coefficients[1], 0.9807129), slope = c(out.lm$coefficients[2], 
    0.7028644), linetype = as.factor(c(2, 1)))
ggplot(df5, aes(x = Body, y = Brain)) + geom_point(size = 3) + geom_abline(aes(intercept = intercept, 
    slope = slope, linetype = linetype, col = linetype), lwd = 1.3, data = df6, show.legend = TRUE) + 
    scale_colour_manual(name = " ", values = 1:2, labels = c("DeepReg", "LS")) + 
    scale_linetype_manual(name = " ", values = 1:2, labels = c("DeepReg", "LS")) + 
    ggtitle("Animal Weight Data") + theme(text = element_text(size = 20)) + ylab(expression(paste(log[10], 
    "(Brain Weight)"))) + xlab(expression(paste(log[10], "(Body Weight)")))

