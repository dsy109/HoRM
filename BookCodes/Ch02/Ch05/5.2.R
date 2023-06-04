library(ggplot2)
library(HoRM)
library(MASS)
library(aprean3)
library(alr3)

###############################################################################
### Example 5.3.2: Computer Repair Data, cont'd
###############################################################################

data(repair, package = "HoRM")

out.comp <- lm(minutes ~ units - 1, data = repair)
reg.anova(out.comp)

