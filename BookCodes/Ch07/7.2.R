library(ggplot2)
library(HoRM)
library(GGally)
library(MASS)
library(MPV)
library(car)
library(perturb)

###############################################################################
### Example 7.4.2: Tortoise Eggs Data, cont'd
###############################################################################

data(tortoise, package = "HoRM")
attach(tortoise)

tortoise2 <- data.frame(tortoise, length2 = length^2)
tortoise.ctr <- data.frame(clutch, length.ctr = length - mean(length), length2.ctr = (length - 
    mean(length))^2)
length.ctr <- tortoise.ctr$length.ctr

out <- lm(clutch ~ ., data = tortoise2)
vif(out)

out.ctr <- lm(clutch ~ ., data = tortoise.ctr)
vif(out.ctr)

summary(out.ctr)

res <- data.frame(res = out.ctr$residuals, tortoise2, studres = studres(out.ctr), 
    fit = out.ctr$fitted.values)
fn <- function(x) out.ctr$coef[1] + out.ctr$coef[2] * x + out.ctr$coef[3] * x^2

ggplot(res, aes(x = length.ctr, y = clutch)) + geom_point(size = 3) + theme(text = element_text(size = 20)) + 
    xlab("Centered Carapace Length (mm)") + ylab("Clutch Size") + stat_function(fun = fn, 
    col = 2) + ggtitle("Tortoise Eggs Data")




