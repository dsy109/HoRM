library(ggplot2)
library(rgl)
library(AER)
library(survival)
library(truncreg)
library(SMPracticals)
library(KMsurv)

###############################################################################
### Example 18.7.4: Durable Goods Data
###############################################################################

data(tobin, package = "survival")
attach(tobin)
cragg_lm <- lm(durable ~ age + quant, data = tobin)
cragg_lm2 <- lm(durable ~ age + quant, data = tobin, subset = (durable > 0))
cragg_trunc <- truncreg(durable ~ age + quant, data = tobin, subset = (durable > 
    0))
summary(cragg_lm)
summary(cragg_lm2)
summary(cragg_trunc)

AIC(logLik(cragg_lm))
AIC(logLik(cragg_lm2))
AIC(logLik(cragg_trunc))

x <- age
z <- durable
y <- quant
n <- length(x)
ind <- which(durable > 0)
nt <- sum(ind)

x1 <- seq(min(x), max(x), length = 30)
y1 <- seq(min(y), max(y), length = 30)

f <- function(x, y, fit) predict(fit, newdata = data.frame(age = x, quant = y))
g <- function(x, y) x * y * 0
col.p <- sapply(1:n, function(i) ifelse(z[i] > 0, 3, 1))
z1 <- outer(x1, y1, f, fit = cragg_lm)
z2 <- outer(x1, y1, f, fit = cragg_lm2)
z3 <- outer(x1, y1, f, fit = cragg_trunc)
z4 <- outer(x1, y1, g)

plot3d(x, y, z, size = 1, col = 3, alpha = c(0.75), type = "s", box = F, xlab = "Age", 
    ylab = "Liquidity Ratio", zlab = "Goods Purchased")
material3d(col = "gray")
persp3d(x1, y1, z1, col = "red", add = T, alpha = c(0.5))
persp3d(x1, y1, z2, col = "green", add = T, alpha = c(0))
persp3d(x1, y1, z3, col = "lightblue", add = T, alpha = c(0))
persp3d(x1, y1, z4, col = "gray", add = T, alpha = c(0.3))

plot3d(x, y, z, size = 1, col = col.p, alpha = c(0.75), type = "s", box = F, xlab = "Age", 
    ylab = "Liquidity Ratio", zlab = "Goods Purchased")
material3d(col = "gray")
persp3d(x1, y1, z1, col = "red", add = T, alpha = c(0), aspect = F)
persp3d(x1, y1, z2, col = "green", add = T, alpha = c(0.5), aspect = F)
persp3d(x1, y1, z3, col = "lightblue", add = T, alpha = c(0), aspect = F)
persp3d(x1, y1, z4, col = "gray", add = T, alpha = c(0.3), aspect = F)

plot3d(x, y, z, size = 1, col = col.p, alpha = c(0.75), type = "s", box = F, xlab = "Age", 
    ylab = "Liquidity Ratio", zlab = "Goods Purchased")
material3d(col = "gray")
persp3d(x1, y1, z1, col = "red", add = T, alpha = c(0), aspect = F)
persp3d(x1, y1, z2, col = "green", add = T, alpha = c(0), aspect = F)
persp3d(x1, y1, z3, col = "lightblue", add = T, alpha = c(0.5), aspect = F)
persp3d(x1, y1, z4, col = "gray", add = T, alpha = c(0.3), aspect = F)
