library(ggplot2)
library(HoRM)
library(VGAM)
library(MASS)
library(sBIC)
library(systemfit)
library(AER)
library(plm)

###############################################################################
### Example 22.5.2: Education Expenditure Data
###############################################################################

data(Grunfeld, package = "AER")
options(digits = 2)
g1 <- subset(Grunfeld, firm %in% c("General Motors", "Goodyear"))
g2 <- plm.data(g1, c("firm", "year"))
g2_sur <- systemfit(invest ~ value + capital, method = "SUR", data = g2)
summary(g2_sur, residCov = FALSE)

