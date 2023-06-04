library(ggplot2)
library(HoRM)
library(aprean3)
library(DescTools)
library(Hmisc)

###############################################################################
### Example 2.8.3: Computer Repair Data
###############################################################################

data(repair, package = "HoRM")
attach(repair)

ggplot(repair, aes(x = units, y = minutes)) + geom_point(size = 3) + geom_smooth(method = lm, 
    se = FALSE) + ggtitle("Computer Repair Data") + theme(text = element_text(size = 20)) + 
    xlab("Units") + ylab("Minutes") + scale_x_continuous(breaks = c(2, 4, 6, 8, 10))

out <- lm(minutes ~ units - 1)
summary(out)

cor(minutes, units, method = "pearson")
cor(minutes, units, method = "spearman")
cor(minutes, units, method = "kendall")
hoeffd(cbind(minutes, units))$D
KendallTauA(minutes, units)
SomersDelta(minutes, units)
GoodmanKruskalGamma(minutes, units)
