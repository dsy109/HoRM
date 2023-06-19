library(ggplot2)
library(rgl)
library(AER)
library(survival)
library(truncreg)
library(SMPracticals)
library(KMsurv)

###############################################################################
### Example 18.7.1: Extramarital Affairs Data
###############################################################################

data(Affairs, package = "AER")
tobit.fit <- tobit(affairs ~ age + yearsmarried + religiousness + rating, data = Affairs)
summary(tobit.fit)

