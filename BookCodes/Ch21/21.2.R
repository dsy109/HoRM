library(ggplot2)
library(HoRM)
library(betareg)
library(HSAUR2)
library(gee)
library(MuMIn)

###############################################################################
### Example 21.6.2: Canadian Auto Insurance Data
###############################################################################

data(Auto, package = "HoRM")

out <- glm(Cost/Claims ~ Premium + Insured, data = Auto, family = inverse.gaussian(), 
    weights = Claims)
summary(out)

df4 <- data.frame(fitted = out$fitted.values, res = residuals(out, type = "deviance"))
ggplot(data = df4, aes(x = fitted, y = res)) + geom_point(size = 3) + xlab("Fitted Values") + 
    ylab("Deviance Residuals") + ggtitle("Deviance Residuals") + geom_abline(intercept = 0, 
    slope = 0) + theme(text = element_text(size = 20))

df5 <- data.frame(cd = cooks.distance(out), index = seq(1, 20, 1))
ggplot(data = df5, aes(x = index, y = cd)) + geom_bar(stat = "identity", position = "dodge", 
    width = 0.1) + ylab("Cook's Distance") + ggtitle("Cook's Distance") + theme(text = element_text(size = 20)) + 
    xlab("Index")

out2 <- glm(Cost/Claims ~ Premium + Insured, data = Auto[-1, ], family = inverse.gaussian(), 
    weights = Claims)
summary(out2)

df6 <- data.frame(fitted = out2$fitted.values, res = residuals(out2, type = "deviance"))
ggplot(data = df6, aes(x = fitted, y = res)) + geom_point(size = 3) + xlab("Fitted Values") + 
    ylab("Deviance Residuals") + ggtitle("Deviance Residuals") + geom_abline(intercept = 0, 
    slope = 0) + theme(text = element_text(size = 20))

df7 <- data.frame(cd = cooks.distance(out2), index = seq(2, 20, 1))
ggplot(data = df7, aes(x = index, y = cd)) + geom_bar(stat = "identity", position = "dodge", 
    width = 0.1) + ylab("Cook's Distance") + ggtitle("Cook's Distance") + theme(text = element_text(size = 20)) + 
    xlab("Index")

