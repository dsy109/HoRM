library(ggplot2)
library(HoRM)
library(MPV)
library(car)

###############################################################################
### Example 8.6.2: Simulated Partial Leverage Data
###############################################################################

# Model 1
set.seed(100)
X1 <- sort(runif(50, 0, 10))
X2 <- runif(50, 0, 10)
Y1 <- 9 + 7 * X1 + rnorm(50, sd = 5)
model <- "Model 1"

out4 <- lm(Y1 ~ X2)
out5 <- lm(X1 ~ X2)
out6 <- lm(out4$res ~ out5$res - 1)

out1 <- lm(Y1 ~ X1)
out2 <- lm(X2 ~ X1)
out3 <- lm(out1$res ~ out2$res - 1)

h.1 <- out5$res^2/sum(out5$res^2)
h.2 <- out2$res^2/sum(out2$res^2)

model1.part <- data.frame(Y1 = Y1, X1 = X1, X2 = X2, r.X1 = out5$res, r.Y1 = out4$res, 
    r.X2 = out2$res, r.Y2 = out1$res, h.1 = h.1, h.2 = h.2)

ggplot(model1.part, aes(x = r.X1, y = r.Y1)) + geom_point(size = 3) + geom_smooth(method = lm, 
    se = FALSE) + ggtitle(paste("Partial Regression Plot for", model)) + theme(text = element_text(size = 20)) + 
    xlab(expression(r[X["[1]"]])) + ylab(expression(r[Y["[1]"]]))

ggplot(model1.part, aes(x = r.X2, y = r.Y2)) + geom_point(size = 3) + geom_smooth(method = lm, 
    se = FALSE) + ggtitle(paste("Partial Regression Plot for", model)) + theme(text = element_text(size = 20)) + 
    xlab(expression(r[X["[2]"]])) + ylab(expression(r[Y["[2]"]]))

ggplot(model1.part, aes(x = X1, y = h.1)) + geom_point(size = 3) + ggtitle(expression(paste("Partial Leverage Plot for ", 
    X[1]))) + theme(text = element_text(size = 20)) + xlab(expression(X[1])) + ylab(expression(h[1]^"*"))

ggplot(model1.part, aes(x = X2, y = h.2)) + geom_point(size = 3) + ggtitle(expression(paste("Partial Leverage Plot for ", 
    X[2]))) + theme(text = element_text(size = 20)) + xlab(expression(X[2])) + ylab(expression(h[2]^"*"))

# Model 2
set.seed(100)
X1 <- sort(runif(50, 0, 10))
X2 <- runif(50, 0, 10)
Y2 <- 9 + 7 * X1 - 4 * X2 + rnorm(50, sd = 5)
model <- "Model 2"

out4 <- lm(Y2 ~ X2)
out5 <- lm(X1 ~ X2)
out6 <- lm(out4$res ~ out5$res - 1)

out1 <- lm(Y2 ~ X1)
out2 <- lm(X2 ~ X1)
out3 <- lm(out1$res ~ out2$res - 1)

Model.2 <- lm(Y2 ~ X1 + X2)
summary(out)
r_Y.1 <- out4$res
r_X.1 <- out5$res
r_Y.2 <- out1$res
r_X.2 <- out2$res
summary(lm(r_Y.1 ~ r_X.1 - 1))
summary(lm(r_Y.2 ~ r_X.2 - 1))

model2.part <- data.frame(Y1 = Y2, X1 = X1, X2 = X2, r.X1 = out5$res, r.Y1 = out4$res, 
    r.X2 = out2$res, r.Y2 = out1$res, h.1 = h.1, h.2 = h.2)

ggplot(model2.part, aes(x = r.X1, y = r.Y1)) + geom_point(size = 3) + geom_smooth(method = lm, 
    se = FALSE) + ggtitle(paste("Partial Regression Plot for", model)) + theme(text = element_text(size = 20)) + 
    xlab(expression(r[X["[1]"]])) + ylab(expression(r[Y["[1]"]]))

ggplot(model2.part, aes(x = r.X2, y = r.Y2)) + geom_point(size = 3) + geom_smooth(method = lm, 
    se = FALSE) + ggtitle(paste("Partial Regression Plot for", model)) + theme(text = element_text(size = 20)) + 
    xlab(expression(r[X["[2]"]])) + ylab(expression(r[Y["[2]"]]))

# Model 3
set.seed(100)
X1 <- sort(runif(50, 0, 10))
X2 <- runif(50, 0, 10)
Y3 <- 9 + 7 * X1 - 4 * X2 + 7 * X2^2 + rnorm(50, sd = 5)
model <- "Model 3"

out4 <- lm(Y3 ~ X2)
out5 <- lm(X1 ~ X2)
out6 <- lm(out4$res ~ out5$res - 1)

out1 <- lm(Y3 ~ X1)
out2 <- lm(X2 ~ X1)
out3 <- lm(out1$res ~ out2$res - 1)

# Fitting Model 2, but Data Generated from Model 3 Partial Residual Plots
out.2 <- lm(Y3 ~ X1 + X2)
out.3 <- lm(Y3 ~ X1 + X2 + I(X2^2))
summary(out.3)

model3.part <- data.frame(Y1 = Y3, X1 = X1, X2 = X2, r.X1 = out5$res, r.Y1 = out4$res, 
    r.X2 = out2$res, r.Y2 = out1$res, h.1 = h.1, h.2 = h.2, pres.X1 = out.2$res + 
        out.2$coef[2] * X1, pres.X2 = out.2$res + out.2$coef[3] * X2, pres.quad.X1 = out.3$res + 
        out.3$coef[2] * X1, pres.quad.X2 = out.3$res + out.3$coef[3] * X2, pres.quad.X2.2 = out.3$res + 
        out.3$coef[4] * X2^2)

ggplot(model3.part, aes(x = r.X1, y = r.Y1)) + geom_point(size = 3) + geom_smooth(method = lm, 
    se = FALSE) + ggtitle(paste("Partial Regression Plot for", model)) + theme(text = element_text(size = 20)) + 
    xlab(expression(r[X["[1]"]])) + ylab(expression(r[Y["[1]"]]))

ggplot(model3.part, aes(x = r.X2, y = r.Y2)) + geom_point(size = 3) + geom_smooth(method = lm, 
    se = FALSE) + ggtitle(paste("Partial Regression Plot for", model)) + theme(text = element_text(size = 20)) + 
    xlab(expression(r[X["[2]"]])) + ylab(expression(r[Y["[2]"]]))


ggplot(model3.part, aes(x = X1, y = pres.X1)) + geom_point(size = 3) + geom_smooth(method = lm, 
    se = FALSE) + ggtitle("Partial Residual Plot (No Quadratic Term)") + theme(text = element_text(size = 20)) + 
    xlab(expression(X["1"])) + ylab("Partial Residual")

ggplot(model3.part, aes(x = X2, y = pres.X2)) + geom_point(size = 3) + geom_smooth(method = lm, 
    se = FALSE) + ggtitle("Partial Residual Plot (No Quadratic Term)") + theme(text = element_text(size = 20)) + 
    xlab(expression(X["2"])) + ylab("Partial Residual")

ggplot(model3.part, aes(x = X1, y = pres.quad.X1)) + geom_point(size = 3) + geom_smooth(method = lm, 
    se = FALSE) + ggtitle("Partial Residual Plot (Quadratic Term)") + theme(text = element_text(size = 20)) + 
    xlab(expression(X["1"])) + ylab("Partial Residual")

ggplot(model3.part, aes(x = X2, y = pres.quad.X2)) + geom_point(size = 3) + geom_smooth(method = lm, 
    se = FALSE) + ggtitle("Partial Residual Plot (Quadratic Term)") + theme(text = element_text(size = 20)) + 
    xlab(expression(X["2"])) + ylab("Partial Residual")

ggplot(model3.part, aes(x = X2^2, y = pres.quad.X2.2)) + geom_point(size = 3) + geom_smooth(method = lm, 
    se = FALSE) + ggtitle("Partial Residual Plot (Quadratic Term)") + theme(text = element_text(size = 20)) + 
    xlab(expression(X[2]^2)) + ylab("Partial Residual")


