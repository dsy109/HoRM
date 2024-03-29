library(gplm)
library(HoRM)
library(mgcv)
library(car)
library(visreg)
library(plyr)
library(ggplot2)
library(ElemStatLearn)
library(SemiPar)
library(np)
library(gridExtra)
library(grid)
library(MASS)
library(GGally)
library(sfsmisc)

###############################################################################
### Example 23.7.3: Boston Housing Data
###############################################################################

data(Boston, package = "MASS")
boston.ppr.unt <- ppr(medv ~ ., data = Boston, nterms = 4, max.terms = 10)

boston.ppr <- ppr(log(medv) ~ crim + zn + indus + chas + I(nox^2) + I(rm^2) + age + 
    log(dis) + log(rad) + tax + ptratio + black + log(lstat), data = Boston, nterms = 4, 
    max.terms = 10)

untran <- data.frame(x = boston.ppr.unt$fitted.values, y = boston.ppr.unt$residuals)
ggplot(untran, aes(x = x, y = y)) + geom_point(size = 3) + theme(text = element_text(size = 20)) + 
    xlab("Fitted Values") + ylab("Residuals") + geom_hline(yintercept = 0, color = 1, 
    size = 0.6) + ggtitle("Residuals from Projection Pursuit Fit\n(Untransformed Data)")

tran <- data.frame(x = boston.ppr$fitted.values, y = boston.ppr$residuals)
ggplot(tran, aes(x = x, y = y)) + geom_point(size = 3) + theme(text = element_text(size = 20)) + 
    xlab("Fitted Values") + ylab("Residuals") + geom_hline(yintercept = 0, color = 1, 
    size = 0.6) + ggtitle("Residuals from Projection Pursuit Fit\n(Transformed Data)")

mean((summary(boston.ppr.unt)$residuals)^2)
mean((summary(boston.ppr)$residuals)^2)

obj <- ppr_funs(boston.ppr)

ord1 <- order(obj$x[, 1])
ord2 <- order(obj$x[, 2])
ord3 <- order(obj$x[, 3])
ord4 <- order(obj$x[, 4])
df <- data.frame(x1 = obj$x[ord1, 1], y1 = obj$y[ord1, 1], x2 = obj$x[ord2, 2], y2 = obj$y[ord2, 
    2], x3 = obj$x[ord3, 3], y3 = obj$y[ord3, 3], x4 = obj$x[ord4, 4], y4 = obj$y[ord4, 
    4])

ggplot(df, aes(x = x1, y = y1)) + geom_point() + geom_line() + xlab("Term 1") + ylab("") + 
    ggtitle("Ridge Function (Term 1)") + theme(text = element_text(size = 20))
ggplot(df, aes(x = x2, y = y2)) + geom_point() + geom_line() + xlab("Term 2") + ylab("") + 
    ggtitle("Ridge Function (Term 2)") + theme(text = element_text(size = 20))
ggplot(df, aes(x = x3, y = y3)) + geom_point() + geom_line() + xlab("Term 3") + ylab("") + 
    ggtitle("Ridge Function (Term 3)") + theme(text = element_text(size = 20))
ggplot(df, aes(x = x4, y = y4)) + geom_point() + geom_line() + xlab("Term 4") + ylab("") + 
    ggtitle("Ridge Function (Term 4)") + theme(text = element_text(size = 20))

