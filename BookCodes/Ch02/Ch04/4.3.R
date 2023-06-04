library(ggplot2)
library(MASS)
library(aprean3)
library(fueleconomy)
library(car)
library(nortest)
library(lmtest)
library(dispmod)

###############################################################################
### Example 4.7.3: Black Cherry Tree Data
###############################################################################

data(minitab, package = "dispmod")

reg2 <- lm(V ~ D, data = minitab)
res <- data.frame(res = studres(reg2))

ggplot(minitab, aes(x = D, y = V)) + geom_point(size = 3) + geom_smooth(method = lm, 
    se = FALSE) + ggtitle("Black Cherry Tree Data") + theme(text = element_text(size = 20)) + 
    xlab("Diameter") + ylab("Volume")

ggplot(res, aes(sample = res)) + stat_qq(size = 3) + geom_abline(intercept = 0, slope = 1, 
    color = 4) + theme(text = element_text(size = 20)) + xlab("Theoretical Quantiles") + 
    ylab("Sample Quantiles") + ggtitle("Normal Q-Q Plot")

bc <- boxcox(reg2, plotit = FALSE)
lambdahat <- bc$x[which.max(bc$y)]
lambdahat
BC.out <- data.frame(sapply(bc, cbind))
CI <- max(bc$y) - qchisq(0.95, 1)/2
ind <- rank(abs(CI - bc$y))
CI.ll <- bc$x[which(ind == 1 | ind == 2)]

transfer.y <- minitab$V^lambdahat
new.cons <- data.frame(minitab, transfer.y)
new.model <- lm(transfer.y ~ D, new.cons)
res.new <- data.frame(res = studres(new.model))

ggplot(BC.out, aes(x = x, y = y)) + geom_line() + ggtitle("Box-Cox Values") + theme(text = element_text(size = 20)) + 
    xlab(expression(lambda)) + ylab("Profile Loglikelihood") + geom_vline(xintercept = lambdahat, 
    color = "red") + geom_vline(xintercept = CI.ll, color = "red", linetype = "longdash") + 
    geom_hline(yintercept = CI, color = "red", linetype = "longdash")

ggplot(new.cons, aes(x = D, y = transfer.y)) + geom_point(size = 3) + geom_smooth(method = lm, 
    se = FALSE) + ggtitle("Transformed Black Cherry Tree Data") + theme(text = element_text(size = 20)) + 
    xlab("Diameter") + ylab("Volume")

ggplot(res.new, aes(sample = res)) + stat_qq(size = 3) + geom_abline(intercept = 0, 
    slope = 1, color = 4) + theme(text = element_text(size = 20)) + xlab("Theoretical Quantiles") + 
    ylab("Sample Quantiles") + ggtitle("Normal Q-Q Plot")

