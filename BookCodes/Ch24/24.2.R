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
### Example 24.5.2: Pima Indian Diabetes Data
###############################################################################

# CART, MARS, Neural Network
data(Pima.tr, package = "MASS")
data(Pima.te, package = "MASS")

Pima.tr.1 <- data.frame(Pima.tr[, -8], type = as.numeric(Pima.tr[, 8]) - 1)
Pima.te.1 <- data.frame(Pima.te[, -8], type = as.numeric(Pima.te[, 8]) - 1)

pima.tree <- rpart(type ~ ., data = Pima.tr, method = "class")
prp(pima.tree, col = "gray", uniform = TRUE)
full.df <- data.frame(predicted = apply(predict(pima.tree, newdata = Pima.te), 1, 
    which.max), truth = Pima.te[, 8])
full.df.tabs <- xtabs(~predicted + truth, data = full.df)
colnames(full.df.tabs) <- rownames(full.df.tabs) <- c("No", "Yes")
full.df.tabs <- round(full.df.tabs/sum(full.df.tabs), 4)
full.df.tabs
1 - sum(diag(full.df.tabs))

opt.tree <- pima.tree$cptable[which.min(pima.tree$cptable[, "xerror"]), "CP"]
pima.ptree <- prune(pima.tree, cp = opt.tree)
prp(pima.ptree, col = "gray", uniform = TRUE)
prune.df <- data.frame(predicted = apply(predict(pima.ptree, newdata = Pima.te), 
    1, which.max), truth = Pima.te[, 8])
prune.df.tabs <- xtabs(~predicted + truth, data = prune.df)
colnames(prune.df.tabs) <- rownames(prune.df.tabs) <- c("No", "Yes")
prune.df.tabs <- round(prune.df.tabs/sum(prune.df.tabs), 4)
prune.df.tabs
1 - sum(diag(prune.df.tabs))

pima.earth <- earth(type ~ ., data = Pima.tr, glm = list(family = binomial))
summary(pima.earth, digits = 2, style = "pmax")  #npreg, bp, and skin unused
net.earth <- data.frame(predicted = round(c(predict(pima.earth, type = "response", 
    newdata = Pima.te))), truth = Pima.te.1[, 8])
net.earth.tabs <- xtabs(~predicted + truth, data = net.earth)
colnames(net.earth.tabs) <- rownames(net.earth.tabs) <- c("No", "Yes")
net.earth.tabs <- round(net.earth.tabs/sum(net.earth.tabs), 4)
net.earth.tabs
1 - sum(diag(net.earth.tabs))

m <- 100
GRID.glu <- data.frame(2, seq(min(Pima.tr[, 2]), max(Pima.tr[, 2]), length = m), 
    70, 29, 32.8, 0.3725, 28)
GRID.bmi <- data.frame(2, 120, 70, 29, seq(min(Pima.tr[, 5]), max(Pima.tr[, 5]), 
    length = m), 0.3725, 28)
GRID.ped <- data.frame(2, 120, 70, 29, 32.8, seq(min(Pima.tr[, 6]), max(Pima.tr[, 
    6]), length = m), 28)
GRID.age <- data.frame(2, 120, 70, 29, 32.8, 0.3725, seq(min(Pima.tr[, 7]), max(Pima.tr[, 
    7]), length = m))
names(GRID.glu) <- names(GRID.bmi) <- names(GRID.ped) <- names(GRID.age) <- names(Pima.tr)[-8]
pred.glu <- predict(pima.earth, type = "earth", newdata = GRID.glu)
pred.bmi <- predict(pima.earth, type = "earth", newdata = GRID.bmi)
pred.ped <- predict(pima.earth, type = "earth", newdata = GRID.ped)
pred.age <- predict(pima.earth, type = "earth", newdata = GRID.age)

dd <- data.frame(x1 = GRID.glu[, 2], x2 = GRID.bmi[, 5], x3 = GRID.ped[, 6], x4 = GRID.age[, 
    7], y1 = pmax(0, pred.glu), y2 = pmax(0, pred.bmi), y3 = pmax(0, pred.ped), y4 = pmax(0, 
    pred.age), rug1 = Pima.tr$glu, rug2 = Pima.tr$bmi, rug3 = Pima.tr$ped, rug4 = Pima.tr$age)

ggplot(data = dd, aes(x = x1, y = y1)) + geom_line() + geom_rug(data = dd, aes(x = rug1, 
    y = rug1), sides = "b", col = "blue") + ylim(c(0, 1)) + xlab("Glucose") + ylab("Probability of Diabetic") + 
    theme(text = element_text(size = 20))

ggplot(data = dd, aes(x = x2, y = y2)) + geom_line() + geom_rug(data = dd, aes(x = rug2, 
    y = rug2), sides = "b", col = "blue") + ylim(c(0, 1)) + xlab("BMI") + ylab("Probability of Diabetic") + 
    theme(text = element_text(size = 20))

ggplot(data = dd, aes(x = x3, y = y3)) + geom_line() + geom_rug(data = dd, aes(x = rug3, 
    y = rug3), sides = "b", col = "blue") + ylim(c(0, 1)) + xlab("Pedigree") + ylab("Probability of Diabetic") + 
    theme(text = element_text(size = 20))

ggplot(data = dd, aes(x = x4, y = y4)) + geom_line() + geom_rug(data = dd, aes(x = rug4, 
    y = rug4), sides = "b", col = "blue") + ylim(c(0, 1)) + xlab("Age") + ylab("Probability of Diabetic") + 
    theme(text = element_text(size = 20))

set.seed(10)
print(pima.net <- neuralnet(type ~ glu + bmi + ped + age, data = Pima.tr.1, hidden = 1, 
    rep = 5))
plot(pima.net, rep = "best")

set.seed(10)
print(pima.net2 <- neuralnet(type ~ glu + bmi + ped + age, data = Pima.tr.1, hidden = 2, 
    rep = 5))
plot(pima.net2, rep = "best")

pima.pred <- compute(pima.net, Pima.te.1[, c(2, 5:7)])
net.df <- data.frame(predicted = round(pima.pred$net.result), truth = Pima.te.1[, 
    8])
net.df.tabs <- xtabs(~predicted + truth, data = net.df)
colnames(net.df.tabs) <- rownames(net.df.tabs) <- c("No", "Yes")
net.df.tabs <- round(net.df.tabs/sum(net.df.tabs), 4)
net.df.tabs
1 - sum(diag(net.df.tabs))

pima.pred2 <- compute(pima.net2, Pima.te.1[, c(2, 5:7)])
net.df2 <- data.frame(predicted = round(pima.pred2$net.result), truth = Pima.te.1[, 
    8])
net.df2.tabs <- xtabs(~predicted + truth, data = net.df2)
colnames(net.df2.tabs) <- rownames(net.df2.tabs) <- c("No", "Yes")
net.df2.tabs <- round(net.df2.tabs/sum(net.df2.tabs), 4)
net.df2.tabs
1 - sum(diag(net.df2.tabs))

