library(ggplot2)
library(MASS)
library(aprean3)
library(fueleconomy)
library(car)
library(nortest)
library(lmtest)
library(dispmod)

###############################################################################
### Example 4.7.1: Steam Output Data, cont'd
###############################################################################

data(dsa01a, package = "aprean3")

temp <- dsa01a$x8
steam <- dsa01a$x1
out <- lm(steam ~ temp)
summary(out)
fit.res <- residuals(out)
sortedres <- sort(fit.res)
n <- length(fit.res)

# (1) Histogram of residuals
ggplot(dsa01a, aes(x = fit.res)) + geom_histogram(binwidth = 0.5, aes(y = ..density.., 
    fill = ..density..)) + theme(text = element_text(size = 20)) + xlab("Residuals") + 
    ylab("Density") + ggtitle("Histogram of Residuals")

# (2) QQ plot of raw residuals
ggplot(dsa01a, aes(sample = fit.res)) + stat_qq(size = 3) + geom_abline(intercept = 0, 
    slope = 1, color = 4) + theme(text = element_text(size = 20)) + xlab("Theoretical Quantiles") + 
    ylab("Sample Quantiles") + ggtitle("Normal Q-Q Plot")

# (3) scatterplot of raw residuals vs. fitted values
ggplot(dsa01a, aes(fitted.values(out), residuals(out))) + geom_point(size = 3) + 
    theme(text = element_text(size = 20)) + xlab("Fitted Values") + ylab("Residuals") + 
    geom_hline(yintercept = 0, color = 1, size = 0.6) + ggtitle("Raw Residuals vs. Fitted Values")

# (4) scatterplot of Studentized residuals vs. fitted values
ggplot(dsa01a, aes(fitted.values(out), studres(out))) + geom_point(size = 3) + theme(text = element_text(size = 20)) + 
    xlab("Fitted Values") + ylab("Residuals") + geom_hline(yintercept = 0, color = 1, 
    size = 0.6) + ggtitle("Studentized Residuals vs. Fitted Values")

# Anderson-Darling test statistic
ad.test(fit.res)

# Kolmogorov-Smirnov test statistic
ks.test(x = fit.res, y = pnorm)

# Shapiro-Wilk test statistic
shapiro.test(fit.res)

# Ryan-Joiner test statistic
z.res <- (sortedres - mean(sortedres))/sd(sortedres)
sum(sortedres * z.res)/sqrt(anova(out)[2, 3] * (n - 1) * sum(z.res^2))

# Chi Square GOF test 4 bins
observed1 <- table(cut(steam, breaks = 4))
expected1 <- table(cut(out$fitted.values, breaks = c(6.35, 7.9, 9.44, 11, 12.5)))
chi.4 <- sum((observed1 - expected1)^2/expected1)
c(chi.4, pchisq(chi.4, 1, lower.tail = F))

# 5 bins
observed2 <- table(cut(steam, breaks = 5))
expected2 <- table(cut(out$fitted.values, breaks = c(6.35, 7.59, 8.82, 10.1, 11.3, 
    12.5)))
chi.5 <- sum((observed2 - expected2)^2/expected2)
c(chi.5, pchisq(chi.5, 2, lower.tail = F))

m1 <- m2 <- m3 <- m4 <- 0
for (i in 1:n)
{
    m1 <- m1 + sum((fit.res[i] - mean(fit.res))^1)/n
    m2 <- m2 + sum((fit.res[i] - mean(fit.res))^2)/n
    m3 <- m3 + sum((fit.res[i] - mean(fit.res))^3)/n
    m4 <- m4 + sum((fit.res[i] - mean(fit.res))^4)/n
}
# skewness of residuals
m3/(m2^(3/2))

# kurtosis of residuals
m4/m2^2

var.tests <- data.frame(temp = temp, fit.res = fit.res, group = factor((fit.res <= 
    median(fit.res)) + 1))

# F-test
sds <- c(by(var.tests$fit.res, var.tests$group, var))
F.var <- sds[1]/sds[2]
c(F.var, pf(F.var, 11, 12, lower.tail = FALSE))

# Bartlett's test
bartlett.test(fit.res ~ group, data = var.tests)

# Breush-Pagan test
bptest(out)

