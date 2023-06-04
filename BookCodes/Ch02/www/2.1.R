library(ggplot2)
library(HoRM)
library(aprean3)
library(DescTools)
library(Hmisc)

###############################################################################
### Example 2.8.1: Toy Data
###############################################################################

data(toy, package = "HoRM")

out <- lm(y ~ x, data = toy)
out$fitted
out$res

ggplot(toy, aes(x = x, y = y)) + geom_point(size = 3) + geom_smooth(method = lm, 
    se = FALSE) + ggtitle("Toy Data") + theme(text = element_text(size = 20))

