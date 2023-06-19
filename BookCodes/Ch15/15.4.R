library(ggplot2)
library(HoRM)
library(RColorBrewer)
library(lme4)
library(car)
library(rsm)
library(daewr)
library(mixexp)

###############################################################################
### Example 15.5.4: Yarn Fiber Data
###############################################################################

data(yarn, package = "HoRM")
DesignPoints(yarn, axislabs = c("Polyethylene", "Polystyrene", "Polypropylene"))

MixturePlot(des = yarn, mod = 2, x1lab = "Fraction Polyethylene", x2lab = "Fraction Polystyrene", 
    x3lab = "Fraction Polypropylene")
MixturePlot(des = yarn, mod = 1, x1lab = "Fraction Polyethylene", x2lab = "Fraction Polystyrene", 
    x3lab = "Fraction Polypropylene")

out <- lm(y ~ x1 + x2 + x3 + x1:x2 + x1:x3 + x2:x3 - 1, data = yarn)
out.red <- lm(y ~ x1 + x2 + x3 - 1, data = yarn)

anova(out.red, out)
summary(out)

