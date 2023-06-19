library(ggplot2)
library(HoRM)
library(betareg)
library(HSAUR2)
library(gee)
library(MuMIn)

###############################################################################
### Example 21.6.1: Fruit Fly Data
###############################################################################

data(fly, package = "HoRM")

ggplot(fly, aes(temp, duration)) + geom_point(size = 3) + ggtitle("Fruit Fly Data") + 
    xlab(expression(paste("Temperature (", degree, "C)"))) + ylab("Duration (hours)") + 
    theme(text = element_text(size = 20))

fn <- function(delta, fly, batch) glm(duration ~ temp + I(1/(temp - delta)), data = fly, 
    family = Gamma(link = "inverse"), weights = batch)$deviance
delta.out <- optim(0, fn, fly = fly, method = "Brent", lower = 0, upper = 50, hessian = TRUE)
delta.hat <- delta.out$par
delta.se <- sqrt((delta.out$hessian/2)^(-1))
gamma.out <- glm(duration ~ temp + I(1/(temp - delta.hat)), data = fly, family = Gamma(link = "inverse"), 
    weights = batch)
summary(gamma.out)
x <- seq(14.5, 32.5, length = 100)
y.hat <- predict(gamma.out, newdata = data.frame(temp = x), type = "response")

df1 <- data.frame(x = x, y.hat = y.hat)
ggplot(data = fly, aes(x = temp, y = duration)) + geom_point(size = 3) + ggtitle("Fruit Fly Data") + 
    xlab(expression(paste("Temperature (", degree, "C)"))) + ylab("Duration (hours)") + 
    geom_line(data = df1, aes(x = x, y = y.hat), col = "red") + theme(text = element_text(size = 20))

df2 <- data.frame(fitted = gamma.out$fitted.values, res1 = residuals(gamma.out, type = "deviance"), 
    res2 = residuals(gamma.out, type = "pearson"))
ggplot(data = df2, aes(x = fitted, y = res1)) + geom_point(size = 3) + xlab("Fitted Values") + 
    ggtitle("Deviance Residuals") + ylab("Deviance Residuals") + geom_abline(intercept = 0, 
    slope = 0) + theme(text = element_text(size = 20))

ggplot(data = df2, aes(x = fitted, y = res2)) + geom_point(size = 3) + xlab("Fitted Values") + 
    ggtitle("Pearson Residuals") + ylab("Pearson Residuals") + geom_abline(intercept = 0, 
    slope = 0) + theme(text = element_text(size = 20))

df3 <- data.frame(cd = cooks.distance(gamma.out), index = seq(1, 23, 1))
ggplot(data = df3, aes(x = index, y = cd)) + geom_bar(stat = "identity", position = "dodge", 
    width = 0.1) + ylab("Cook's Distance") + xlab("Index") + theme(text = element_text(size = 20)) + 
    ggtitle("Cook's Distance")

