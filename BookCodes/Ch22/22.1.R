library(ggplot2)
library(HoRM)
library(VGAM)
library(MASS)
library(sBIC)
library(systemfit)
library(AER)
library(plm)

###############################################################################
### Example 22.5.1: Amitriptyline Data
###############################################################################

data(amit, package = "HoRM")

fits <- manova(cbind(TOT, AMI) ~ ., data = amit)
coef(fits)
out <- summary.aov(fits)
mvreg.out <- lapply(out, reg.manova)
mvreg.out

out.mv <- lm(cbind(TOT, AMI) ~ ., data = amit)
summary(out.mv)

SSCP.fn(fits)

summary(fits, test = "Pillai")
summary(fits, test = "Wilks")
summary(fits, test = "Hotelling-Lawley")
summary(fits, test = "Roy")

dataframe <- data.frame(fits$fitted.values, studres(fits)[1:17], studres(fits)[18:34])

ggplot(dataframe, aes(x = dataframe[, 1], y = dataframe[, 3])) + geom_point(size = 3) + 
    ggtitle("Response: Total TCAD Plasma Level") + xlab("Fitted Values") + ylab("Studentized Residuals") + 
    geom_hline(yintercept = 0, color = 1, size = 0.6) + theme(text = element_text(size = 20))

ggplot(dataframe, aes(x = dataframe[, 2], y = dataframe[, 4])) + geom_point(size = 3) + 
    ggtitle("Response: Amitriptyline") + xlab("Fitted Values") + ylab("Studentized Residuals") + 
    geom_hline(yintercept = 0, color = 1, size = 0.6) + theme(text = element_text(size = 20))

new.x <- c(1, 1, 2000, 200, 50, 100)

# confidence region
sum_hat <- 1/17 * t(fits$residuals) %*% fits$residuals
x <- as.matrix(cbind(1, amit$GEN, amit$AMT, amit$PR, amit$DIAP, amit$QRS))

# i = 1
t(new.x) %*% fits$coefficients[, 1] + c(-1, 1) * sqrt(pf(q = 0.95, df1 = 1, df2 = 9)) * 
    sqrt(t(new.x) %*% solve(t(x) %*% x) %*% new.x * 17/9 * diag(sum_hat)[1])
# i = 2
t(new.x) %*% fits$coefficients[, 1] + c(-1, 1) * sqrt(2 * 9/8 * pf(q = 0.95, df1 = 2, 
    df2 = 8)) * sqrt(t(new.x) %*% solve(t(x) %*% x) %*% new.x * 17/9 * diag(sum_hat)[2])

# prediction region i = 1
t(new.x) %*% fits$coefficients[, 1] + c(-1, 1) * sqrt(pf(q = 0.95, df1 = 1, df2 = 9)) * 
    sqrt((t(new.x) %*% solve(t(x) %*% x) %*% new.x + 1) * 17/9 * diag(sum_hat)[1])

# prediction region i = 2
t(new.x) %*% fits$coefficients[, 1] + c(-1, 1) * sqrt(2 * 9/8 * pf(q = 0.95, df1 = 2, 
    df2 = 8)) * sqrt((t(new.x) %*% solve(t(x) %*% x) %*% new.x + 1) * 17/9 * diag(sum_hat)[2])

rreg <- ReducedRankRegressions(numResponses = 2, numCovariates = 5, maxRank = 2)
GEN <- as.numeric(levels(amit[, 3]))[amit[, 3]]
amit1 <- amit
amit1$GEN <- GEN
amit2 <- list(X = t(amit1[, 3:7]), Y = t(amit1[, 1:2]))
out.rr <- sBIC(amit2, mod = rreg)
out.rr

rr.fit1 <- rrvglm(cbind(TOT, AMI) ~ ., family = gaussianff, data = amit, Rank = 1)
Coef(rr.fit1)

