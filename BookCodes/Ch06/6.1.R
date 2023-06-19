library(ggplot2)
library(HoRM)
library(MASS)
library(MPV)
library(StatDA)
library(rgl)
library(tolerance)

###############################################################################
### Example 6.7.1: Thermal Energy Data
###############################################################################

data(solar, package = "MPV")
names(solar) <- c("heat.flux", "insolation", "east", "south", "north", "time")

cor(solar[, c(1, 3:5)])

out1 <- lm(heat.flux ~ east + south + north, data = solar)
mlr.1 <- data.frame(res = out1$residuals, solar, studres = studres(out1), fit = out1$fitted.values)

ggplot(mlr.1, aes(x = studres)) + geom_histogram(binwidth = 0.5, aes(y = ..density.., 
    fill = ..density..)) + theme(text = element_text(size = 20)) + xlab("Residuals") + 
    ylab("Density") + ggtitle("Histogram of Studentized Residuals")

ggplot(mlr.1, aes(x = fit, y = studres)) + geom_point(size = 3) + theme(text = element_text(size = 20)) + 
    xlab("Fitted Values") + ylab("Studentized Residuals") + geom_hline(yintercept = 0, 
    color = 1, size = 0.6) + ggtitle("Scatterplot of Studentized Residuals")

summary(out1)

out2 <- lm(heat.flux ~ south + north, data = solar)
mlr.2 <- data.frame(res = out2$residuals, solar, studres = studres(out2), fit = out2$fitted.values)

ggplot(mlr.2, aes(x = studres)) + geom_histogram(binwidth = 0.5, aes(y = ..density.., 
    fill = ..density..)) + theme(text = element_text(size = 20)) + xlab("Residuals") + 
    ylab("Density") + ggtitle("Histogram of Studentized Residuals")

ggplot(mlr.2, aes(x = fit, y = studres)) + geom_point(size = 3) + theme(text = element_text(size = 20)) + 
    xlab("Fitted Values") + ylab("Studentized Residuals") + geom_hline(yintercept = 0, 
    color = 1, size = 0.6) + ggtitle("Scatterplot of Studentized Residuals")

summary(out2)

vcov(out2)
sqrt(diag(vcov(out2)))

cov2cor(vcov(out2))

out2$coef[2] + c(-1, 1) * qt(0.975, 26) * summary(out2)$coef[2, 2]
out2$coef[3] + c(-1, 1) * qt(0.975, 26) * summary(out2)$coef[3, 2]

newx1 <- c(35, 40)
newx2 <- c(17, 16)
MSE <- anova(out2)$"Mean Sq"[3]
n <- length(solar$south)
y.hat <- predict(out2, newdata = data.frame(south = newx1, north = newx2))
X.matrix <- cbind(1, solar$south, solar$north)
x.h.1 <- matrix(data = c(1, 35, 17))
x.h.2 <- matrix(data = c(1, 40, 16))
se.y.hat.1 <- sqrt(MSE * (t(x.h.1) %*% solve(t(X.matrix) %*% X.matrix) %*% x.h.1))
se.y.hat.2 <- sqrt(MSE * (t(x.h.2) %*% solve(t(X.matrix) %*% X.matrix) %*% x.h.2))

# 90% CIs
predict(out2, newdata = data.frame(south = newx1, north = newx2), level = 0.9, interval = "confidence")

# Bonferroni Joint 90% CIs
y.hat[1] + c(-1, 1) * qt(1 - 0.1/(2 * 3), 26) * se.y.hat.1
y.hat[2] + c(-1, 1) * qt(1 - 0.1/(2 * 3), 26) * se.y.hat.2

# 90% Working-Hotelling CBs
y.hat[1] + c(-1, 1) * se.y.hat.1 * sqrt(2 * qf(1 - 0.1, 3, 26))
y.hat[2] + c(-1, 1) * se.y.hat.2 * sqrt(2 * qf(1 - 0.1, 3, 26))

# 90% PIs
predict(out2, newdata = data.frame(south = newx1, north = newx2), level = 0.9, interval = "prediction")

# Bonferroni Joint 90% PIs
y.hat[1] + c(-1, 1) * qt(1 - 0.1/(2 * 3), 26) * sqrt(MSE + (se.y.hat.1)^2)
y.hat[2] + c(-1, 1) * qt(1 - 0.1/(2 * 3), 26) * sqrt(MSE + (se.y.hat.2)^2)

# Scheffe Joint 90% PIs
y.hat[1] + c(-1, 1) * sqrt(2 * qf(1 - 0.1, 3, 26) * (MSE + (se.y.hat.1)^2))
y.hat[2] + c(-1, 1) * sqrt(2 * qf(1 - 0.1, 3, 26) * (MSE + (se.y.hat.2)^2))

# 90%/99% One-sided TIs
regtol.int(out2, new.x = matrix(c(35, 40, 17, 16), ncol = 2), alpha = 0.1, P = 0.99, 
    side = 1)[c(9, 31), ]

# 90%/99% Two-sided TIs
regtol.int(out2, new.x = matrix(c(35, 40, 17, 16), ncol = 2), alpha = 0.1, P = 0.99, 
    side = 2)[c(9, 31), ]

