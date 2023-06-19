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
### Example 25.5.1: Tuberculosis Vaccine Literature Data
###############################################################################

data(dat.bcg, package = "metafor")
bcg <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat.bcg)
fe.fit <- mareg(yi ~ alloc, var = vi, method = "FE", data = bcg)
summary(fe.fit)
re.fit <- mareg(yi ~ year + ablat, var = vi, method = "REML", data = bcg)
summary(re.fit)

df7 <- data.frame(x = exp(c(bcg$yi)), y = exp(predict(fe.fit)$pred), Group = c(rep("Random", 
    4), rep("Alternate", 2), rep("Random", 3), rep("Systematic", 4)))
df7$Group <- as.factor(df7$Group)
ggplot(data = df7, aes(x = x, y = y, col = Group, shape = Group)) + geom_point(size = 4) + 
    theme(text = element_text(size = 20)) + ggtitle("Fixed-Effect Meta-Regression Fits") + 
    xlab("Relative Risk") + ylab("Predicted Relative Risk")

df8 <- data.frame(x = exp(c(bcg$yi)), y = exp(predict(re.fit)$pred), Group = c(rep("Random", 
    4), rep("Alternate", 2), rep("Random", 3), rep("Systematic", 4)))
ggplot(data = df8, aes(x = x, y = y, col = Group, shape = Group)) + geom_point(size = 4) + 
    ggtitle("Random-Effects Meta-Regression Fits") + xlab("Relative Risk") + ylab("Predicted Relative Risk") + 
    theme(text = element_text(size = 20))

