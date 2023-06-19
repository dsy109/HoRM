library(ggplot2)
library(GGally)
library(neuralnet)
library(SemiPar)
library(tree)
library(earth)
library(rpart)
library(rpart.plot)
library(MASS)
library(kernlab)
library(randomForest)
library(mboost)
library(gbm)
library(ipred)
library(prim)

###############################################################################
### Example 24.5.3: Boosting and Regression Transfer
###############################################################################

data(Boston, package = "MASS")

lm.out <- lm(medv ~ ., data = Boston)

set.seed(100)
rf.out <- randomForest(medv ~ ., data = Boston)
rf.out
predict(rf.out)

set.seed(100)
boost.out <- gbm(medv ~ ., data = Boston, distribution = "gaussian", shrinkage = 0.05, 
    cv.folds = 5)
boost.out
predict(boost.out, n.trees = 100)

set.seed(100)
bag.out <- bagging(medv ~ ., data = Boston)
bag.out
predict(bag.out)

fits.out <- data.frame(Observed = Boston$medv, `Linear Model` = predict(lm.out), 
    `Random Forest` = predict(rf.out), Boosting = predict(boost.out, n.trees = 100), 
    Bagging = predict(bag.out))

ggscatmat(fits.out) + theme(text = element_text(size = 20)) + stat_smooth(na.rm = TRUE)

x <- Boston[, c(1, 6, 10)]
y <- Boston[, 14]
prim.out <- prim.box(x = x, y = y, threshold.type = 1, peel.alpha = 0.01, paste.alpha = 0.01)
summary(prim.out, print.box = TRUE)

