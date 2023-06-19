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
### Example 25.9.1: Belgian Food Expenditure Data
###############################################################################

data(engel, package = "quantreg")
taus <- c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)
cols <- rep("gray", length(taus))
cols[4] <- "blue"
out.rq <- lapply(1:length(taus), function(i) rq(foodexp ~ income, data = engel, tau = taus[i]))
cbind(taus, t(sapply(1:length(taus), function(i) out.rq[[i]]$coef)))
out.lm <- lm(foodexp ~ income, engel)
out.lm$coefficients

df9 <- data.frame(foodexp = engel$foodexp, income = engel$income)
df10 <- data.frame(t(sapply(1:7, function(i) out.rq[[i]]$coef)), group = c(2, 2, 
    2, 1, 2, 2, 2), col = c(rep("#838B8B", 3), "#0000FF", rep("#838B8B", 3)))
names(df10) <- c("intercept", "slope", "group", "col")
df10 <- rbind(df10, data.frame(intercept = coef(out.lm)[1], slope = coef(out.lm)[2], 
    group = 3, col = "FF0000"))
df10$group <- as.factor(df10$group)
ggplot(data = df9, aes(y = foodexp, x = income)) + geom_point(size = 3) + geom_abline(data = df10, 
    aes(intercept = intercept, slope = slope, group = group, col = col, lty = group), lwd = 1.2) + 
    xlab("Household Income") + ylab("Food Expenditure") + ggtitle("Belgian Food Expenditure Data") + 
    scale_colour_manual(guide = FALSE, values = c("blue", "red", "black")) + theme(text = element_text(size = 20), 
    legend.position = "none")

