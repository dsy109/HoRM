library(ggplot2)
library(HoRM)
library(pROC)
library(lmtest)
library(mlogit)
library(nnet)
library(foreign)
library(pscl)
library(msme)
library(gamlss.tr)
library(VGAM)
library(reshape)

###############################################################################
### Example 20.5.3: Cheese-Tasting Experiment Data
###############################################################################

data(cheese, package = "HoRM")
out.cheese <- polr(Response ~ Cheese, weights = N, data = cheese)
out.cheese.int <- polr(Response ~ 1, weights = N, data = cheese)
summary(out.cheese)

suppressWarnings(exp(cbind(OR = coef(out.cheese), confint(out.cheese))))

lmtest::lrtest(out.cheese.int, out.cheese)

