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
### Example 25.6.1: Census of Agriculture Data
###############################################################################

data(agstrat, package = "SDaA")

t.x.hat <- sum(agstrat[, 4])
t.y.hat <- sum(agstrat[, 3])
B.hat <- t.y.hat/t.x.hat
N <- 3078
t.X <- 964470625
xbar.N <- t.X/N
ybar.ratio <- B.hat * xbar.N
t.y.ratio <- B.hat * t.X

out.lm <- lm(acres92 ~ acres87, data = agstrat)
print(out.lm$coefficients, digits = 5)

t.y.reg <- sum(agstrat[, 3]) + out.lm$coefficients[2] * (t.X - t.x.hat)
ybar.reg <- predict(out.lm, newdata = data.frame(acres87 = xbar.N))

rbind(t.x.hat, t.y.hat, B.hat, t.y.ratio, ybar.ratio, t.y.reg, ybar.reg)

ggplot(agstrat, aes(x = acres87/1e+06, y = acres92/1e+06, color = region, shape = region)) + 
    geom_point(size = 3) + ggtitle("Census of Agriculture Survey Data") + theme(text = element_text(size = 17)) + 
    xlab("1987 Acreage (in Millions)") + ylab("1992 Acreage (in Millions)") + geom_abline(slope = B.hat, 
    intercept = 0) + scale_shape_discrete(name = "Region") + scale_color_discrete(name = "Region")

ag.des <- svydesign(ids = ~0, weights = ~weight, strata = ~region, data = agstrat)

surv.out1 <- svyglm(acres82 ~ largef82 + smallf82 + farms82, design = ag.des)
summary(surv.out1)
surv.out2 <- svyglm(acres82 ~ largef82 + smallf82, design = ag.des)
summary(surv.out2)
new.data <- data.frame(largef82 = agstrat$largef87, smallf82 = agstrat$smallf87)
surv.pred <- predict(surv.out2, newdata = new.data)
cbind(agstrat$acres87, surv.pred)

df <- data.frame(Acres = agstrat$acres87, Predicted = c(surv.pred), Region = as.factor(agstrat$region), 
    se.l = c(surv.pred) - sqrt(vcov(surv.pred)), se.u = c(surv.pred) + sqrt(vcov(surv.pred)))

ggplot(df, aes(x = Acres/1e+06, y = Predicted/1e+06, color = Region, shape = Region)) + 
    geom_point(size = 3) + ggtitle("1982 Census of Agriculture Survey Regression Model") + 
    theme(text = element_text(size = 17)) + xlab("True 1987 Acreage (in Millions)") + 
    ylab("Survey Predicted 1987 Acreage (in Millions)") + geom_errorbar(aes(ymin = se.l/1e+06, 
    ymax = se.u/1e+06)) + geom_abline(slope = 1, intercept = 0)

