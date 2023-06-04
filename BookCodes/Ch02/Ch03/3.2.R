library(ggplot2)
library(HoRM)
library(aprean3)
library(tolerance)
library(psych)
library(investr)

###############################################################################
### Example 3.6.2: Computer Repair Data, cont'd
###############################################################################

data(repair, package = "HoRM")

out1 <- lm(minutes ~ units, data = repair)
summary(out1)

out <- lm(minutes ~ units - 1, data = repair)
summary(out)

out$coef + c(-1, 1) * qt(0.975, 13) * summary(out)$coef[2]

