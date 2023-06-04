library(ggplot2)
library(HoRM)
library(MASS)
library(aprean3)
library(alr3)

###############################################################################
### Example 5.3.1: Steam Output Data, cont'd
###############################################################################

data(dsa01a, package = "aprean3")

temp <- dsa01a$x8
steam <- dsa01a$x1
out <- lm(steam ~ temp)
reg.anova(out)
