library(ggplot2)
library(MASS)
library(aprean3)
library(fueleconomy)
library(car)
library(nortest)
library(lmtest)
library(dispmod)

###############################################################################
### Example 4.7.2: 1993 Car Sale Data
###############################################################################

data("Cars93", package = "MASS")

red.data <- Cars93[, c(7, 12)]
reg <- lm(MPG.city ~ EngineSize, data = red.data)
reg1 <- lm(log(MPG.city) ~ log(EngineSize), data = red.data)

ggplot(red.data, aes(red.data$EngineSize, red.data$MPG.city)) + geom_point(size = 3) + 
    geom_smooth(method = lm, se = FALSE) + theme(text = element_text(size = 20)) + 
    xlab("Engine Size (L)") + ylab("City Mileage (MPG)") + ggtitle("1993 Car Sale Data")

ggplot(red.data, aes(fitted.values(reg), studres(reg))) + geom_point(size = 3) + 
    ggtitle("Studentized Residuals vs. Fitted Values") + theme(text = element_text(size = 20)) + 
    xlab("Fitted Values") + ylab("Studentized Residuals") + geom_hline(yintercept = 0, 
    color = 1, size = 0.6)

ggplot(red.data, aes(x = log(red.data$EngineSize), y = log(red.data$MPG.city))) + 
    geom_point(size = 3) + geom_smooth(method = lm, se = FALSE) + theme(text = element_text(size = 20)) + 
    xlab("log(Engine Size)") + ylab("log(City Mileage)") + ggtitle("Transformed 1993 Car Sale Data")

ggplot(red.data, aes(fitted.values(reg1), studres(reg1))) + geom_point(size = 3) + 
    ggtitle("Studentized Residuals vs. Fitted Values") + theme(text = element_text(size = 20)) + 
    xlab("Fitted Values") + ylab("Studentized Residuals") + geom_hline(yintercept = 0, 
    color = 1, size = 0.6)

car.res <- residuals(reg1)
car.res <- data.frame(car.res, group = as.factor((car.res <= median(car.res)) + 1))

leveneTest(car.res ~ group, data = car.res, center = mean)
leveneTest(car.res ~ group, data = car.res, center = median)
leveneTest(car.res ~ group, data = car.res, center = mean, trim = 0.1)
