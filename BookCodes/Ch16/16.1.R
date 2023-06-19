library(ggplot2)
library(car)
library(MASS)
library(lars)
library(pls)
library(dr)
library(ISLR)
library(lasso2)
library(reshape2)
library(GGally)
library(glmnet)

###############################################################################
### Example 16.5.1: Prostate Cancer Data
###############################################################################

data(Prostate, package = "lasso2")

n <- nrow(Prostate)
set.seed(100)
ind <- sample(1:n, 30, replace = F)
train <- Prostate[-ind, ]
test <- Prostate[ind, ]

ggscatmat(Prostate) + theme(text = element_text(size = 11), 
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Linear model fit
fit.lm <- lm(lpsa ~ ., data = train)
rss.lm <- (summary(fit.lm)$sigma)^2

# Ridge regression fit
select(ridge.sel <- lm.ridge(lpsa ~ ., data = train, lambda = seq(0, 10, 1e-04)))
lambda1 <- ridge.sel$kHKB
lambda1

fit.ridge <- lm.ridge(lpsa ~ ., data = train, lambda = lambda1)
beta.ridge <- coef(fit.ridge)
res.ridge <- train$lpsa - beta.ridge[1] - as.matrix(train[, -9]) %*% beta.ridge[-1]
d <- svd(as.matrix(train[, -9]))$d
df <- n - sum(d^2/(lambda1 + d^2))
rss.ridge <- sum(res.ridge^2)/df

# LASSO fit
fit.lasso <- lars(as.matrix(train[, -9]), as.numeric(train[, 9]), type = "lasso")
cv.lambda <- cv.glmnet(as.matrix(train[, -9]), as.numeric(train[, 9]), nfolds = 10)$lambda.1se
lambda.lasso <- c(fit.lasso$lambda, 0)
beta.lasso.cand <- cbind(mean(train[, 9]), coef(fit.lasso))
lambda2 <- fit.lasso$lambda[which.min(abs(fit.lasso$lambda - cv.lambda))]
beta.lasso <- beta.lasso.cand[which(fit.lasso$lambda == lambda2), ]
p <- sum(beta.lasso != 0) + 1
res.lasso <- train[, 9] - predict(fit.lasso, as.matrix(train[, -9]), s = p, type = "fit")$fit
rss.lasso <- sum(res.lasso^2)/(n - p)

# LARS fit
fit.lars <- lars(as.matrix(train[, -9]), as.numeric(train[, 9]), type = "lar")
cv.lambda <- cv.glmnet(as.matrix(train[, -9]), as.numeric(train[, 9]), nfolds = 10)$lambda.1se
lambda.lars <- c(fit.lars$lambda, 0)
beta.lars.cand <- cbind(mean(train[, 9]), coef(fit.lars))
lambda2 <- fit.lars$lambda[which.min(abs(fit.lars$lambda - cv.lambda))]
beta.lars <- beta.lars.cand[which(fit.lars$lambda == lambda2), ]
p <- sum(beta.lars != 0) + 1
res.lars <- train[, 9] - predict(fit.lars, as.matrix(train[, -9]), s = p, type = "fit")$fit
rss.lars <- sum(res.lars^2)/(n - p)

# FS fit
fit.fs <- lars(as.matrix(train[, -9]), as.numeric(train[, 9]), type = "forward.stagewise")
cv.lambda <- cv.glmnet(as.matrix(train[, -9]), as.numeric(train[, 9]), nfolds = 10)$lambda.1se
lambda.fs <- c(fit.fs$lambda, 0)
beta.fs.cand <- cbind(mean(train[, 9]), coef(fit.fs))
lambda2 <- fit.fs$lambda[which.min(abs(fit.fs$lambda - cv.lambda))]
beta.fs <- beta.fs.cand[which(fit.fs$lambda == lambda2), ]
p <- sum(beta.fs != 0) + 1
res.fs <- train[, 9] - predict(fit.fs, as.matrix(train[, -9]), s = p, type = "fit")$fit
rss.fs <- sum(res.fs^2)/(n - p)

# PC regression fit
X.star <- as.matrix(scale(train[, -9]))
evalues <- eigen(t(X.star) %*% X.star)$values
round(cumsum(evalues/sum(evalues)), 4)
fit.pc <- pcr(lpsa ~ ., data = train, ncomp = 6)
beta.pc <- drop(coef(fit.pc, intercept = T))
res.pc <- drop(fit.pc$resid)[, 6]
rss.pc <- sum(res.pc^2)/(n - 6)

# PLS regression fit
set.seed(100)
cv.pls <- plsr(lpsa ~ ., data = train, ncomp = 6, method = "oscorespls", validation = "CV")
summary(cv.pls)  #Use 4 based on CV
fit.pls <- plsr(lpsa ~ ., data = train, ncomp = 4, method = "oscorespls")
beta.pls <- drop(coef(fit.pls, intercept = T))
res.pls <- drop(fit.pls$resid)[, 2]
rss.pls <- sum(res.pls^2)/(n - 4)

# Prediction sum of squares
pss.lm <- sum((test[, 9] - predict(fit.lm, test[, 1:8]))^2)
pss.ridge <- sum((test[, 9] - beta.ridge[1] - as.matrix(test[, 1:8]) %*% beta.ridge[2:9])^2)
pss.lasso <- sum((test[, 9] - predict(fit.lasso, as.matrix(test[, 1:8]), s = p, type = "fit")$fit)^2)
pss.lars <- sum((test[, 9] - predict(fit.lars, as.matrix(test[, 1:8]), s = p, type = "fit")$fit)^2)
pss.fs <- sum((test[, 9] - predict(fit.fs, as.matrix(test[, 1:8]), s = p, type = "fit")$fit)^2)
pss.pc <- sum((test[, 9] - drop(predict(fit.pc, test[, 1:8], 6)))^2)
pss.pls <- sum((test[, 9] - drop(predict(fit.pls, test[, 1:8], 4)))^2)

RSS <- c(rss.lm, rss.ridge, rss.lasso, rss.lars, rss.fs, rss.pc, rss.pls)
PRESS <- c(pss.lm, pss.ridge, pss.lasso, pss.lars, pss.fs, pss.pc, pss.pls)
SS <- round(rbind(RSS, PRESS), 4)
colnames(SS) <- c("OLS", "Ridge", "LASSO", "LARS", "F-S", "PCR", "PLS")
SS

tmp <- lm.ridge(lpsa ~ ., data = train, lambda = seq(0, 10, 1e-04))
ridgeframe <- data.frame(time = seq(0, 10, 1e-04), coef(tmp)[, -1])
df <- melt(ridgeframe, id.vars = "time", variable.name = "series")
ggplot(df[round(seq(1, nrow(df), length = 1000)), ], aes(x = time, y = value)) + 
    geom_line(size = 1.3, aes(color = series, linetype = series)) + ylab("Coefficients") + 
    xlab(expression(lambda)) + ggtitle("Ridge Trace Estimates") + theme(text = element_text(size = 20)) + 
    geom_vline(xintercept = lambda1, linetype = 2)

frame <- data.frame(time = lambda.lasso, beta.lasso.cand[, -1])
df1 <- melt(frame, id.vars = "time", variable.name = "series")
ggplot(df1, aes(x = time, y = value)) + geom_line(size = 1.3, aes(color = series, 
    linetype = series)) + ylab("Coefficients") + xlab(expression(lambda)) + ggtitle("LASSO Estimates") + 
    theme(text = element_text(size = 20))

