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
### Example 24.5.1: Motorcycle Accident Data
###############################################################################

# SVM
data(mcycle, package = "MASS")

regm <- ksvm(accel ~ times, data = mcycle, epsilon = 0.01, kpar = list(sigma = 16), 
    cross = 3)
regm2 <- ksvm(accel ~ times, data = mcycle, epsilon = 0.1, kpar = list(sigma = 16), 
    cross = 3)
regm3 <- ksvm(accel ~ times, data = mcycle, epsilon = 0.7, kpar = list(sigma = 16), 
    cross = 3)

ggplot(data = mcycle, aes(x = mcycle$times, y = mcycle$accel)) + geom_point(size = 3) + 
    ggtitle("Simulated Motorcycle Data") + xlab("Times") + ylab("Acceleration") + 
    theme(text = element_text(size = 20))

df <- data.frame(x = mcycle[, 1], y1 = predict(regm, data.frame(times = mcycle[, 
    1])), y2 = predict(regm2, data.frame(times = mcycle[, 1])), y3 = predict(regm3, 
    data.frame(times = mcycle[, 1])))

ggplot(data = mcycle, aes(x = mcycle$times, y = mcycle$accel)) + geom_point(size = 3) + 
    ggtitle("SVM Regression Fits") + xlab("Times") + ylab("Acceleration") + geom_line(data = df, 
    aes(x = x, y = y1, col = "red"), lwd = 1.2) + theme(text = element_text(size = 20)) + 
    geom_line(data = df, aes(x = x, y = y2, col = "green"), lwd = 1.2) + geom_line(data = df, 
    aes(x = x, y = y3, col = "blue"), lwd = 1.2) + scale_color_manual(values = c("red", 
    "green", "blue"), name = expression(paste(epsilon, "-Value")), labels = c("0.01", 
    "0.1", "0.7"))

