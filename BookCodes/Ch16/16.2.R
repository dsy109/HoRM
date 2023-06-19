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
### Example 16.5.2: Automobile Features Data
###############################################################################

data(Auto, package = "ISLR")
options(digits = 3)
auto.lm <- lm(mpg ~ . - year - origin - name, data = Auto)
round(vif(auto.lm), 3)
Auto.sir <- dr(mpg ~ . - year - origin - name, data = Auto, method = "sir", nslices = 8)
Auto.save <- dr(mpg ~ . - year - origin - name, data = Auto, method = "save", nslices = 8)
Auto.phd <- dr(mpg ~ . - year - origin - name, data = Auto, method = "phdy", nslices = 8)
Auto.ire <- dr(mpg ~ . - year - origin - name, data = Auto, method = "ire", nslices = 8)

summary(Auto.sir)
summary(Auto.save)
summary(Auto.phd)
summary(Auto.ire)

ggscatmat(Auto, columns = c(1:6)) + theme(text = element_text(size = 11), 
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

