library(ggplot2)
library(HoRM)
library(MASS)
library(MPV)
library(StatDA)
library(rgl)
library(tolerance)

###############################################################################
### Example 6.7.3: Tortoise Eggs Data
###############################################################################

data(tortoise, package = "HoRM")

m1 <- lm(clutch ~ ., data = tortoise, x = TRUE)
summary(m1)
m2 <- lm(clutch ~ length + I(length^2), data = tortoise, x = TRUE)
summary(m2)

res <- data.frame(res = m2$residuals, tortoise, studres = studres(m2), fit = m2$fitted.values)
fn <- function(x) m2$coef[1] + m2$coef[2] * x + m2$coef[3] * x^2

ggplot(res, aes(x = length, y = clutch)) + geom_point(size = 3) + geom_smooth(method = lm, 
    se = FALSE) + theme(text = element_text(size = 20)) + xlab("Carapace Length (mm)") + 
    ylab("Clutch Size") + ggtitle("Tortoise Eggs Data")

ggplot(res, aes(x = length, y = clutch)) + geom_point(size = 3) + theme(text = element_text(size = 20)) + 
    xlab("Carapace Length (mm)") + ylab("Clutch Size") + stat_function(fun = fn, 
    col = 2) + ggtitle("Tortoise Eggs Data")

