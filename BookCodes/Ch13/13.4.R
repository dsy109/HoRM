library(ggplot2)
library(HoRM)
library(quantmod)
library(astsa)
library(orcutt)
library(car)
library(forecast)
library(nlme)
library(HarmonicRegression)
library(prais)
library(lmtest)

###############################################################################
### Example 13.5.4: Air Passengers Data
###############################################################################

data(AirPassengers, package = "datasets")
HW.out1 <- HoltWinters(AirPassengers, seasonal = "multiplicative")  # Holt-Winter, multiplicative
HW.out2 <- HoltWinters(AirPassengers, seasonal = "additive")  # Holt-Winter, additive
HW.out3 <- HoltWinters(AirPassengers, beta = FALSE, gamma = FALSE)  # Double Exponential Smoothing
HW.out4 <- HoltWinters(AirPassengers, gamma = FALSE)  # Exponential Smoothing

HW.out1[3:5]
HW.out2[3:5]
HW.out3[3:5]
HW.out4[3:5]

ggplot(AirPassengers, aes(x = seq(1949, 1960, 1/13), y = matrix(AirPassengers))) + 
    geom_line(lwd = 1.1, col = "#56B4E9") + geom_line(data = AirPassengers, aes(x = seq(1949, 1960, 
    1/13), y = c(rep(NA, 2), matrix(fitted(HW.out4)[, 1]))), col = "#000000", linetype = "dashed", 
    lwd = 1.1) + theme(text = element_text(size = 20)) + xlab("Year") + ylab("Number of Passengers") + 
    ggtitle("Exponential Smoothing")

ggplot(AirPassengers, aes(x = seq(1949, 1960, 1/13), y = matrix(AirPassengers))) + 
    geom_line(lwd = 1.1, col = "#56B4E9") + geom_line(data = AirPassengers, aes(x = seq(1949, 1960, 
    1/13), y = c(rep(NA, 1), matrix(fitted(HW.out3)[, 1]))), col = "#000000", linetype = "dashed", 
    lwd = 1.1) + theme(text = element_text(size = 20)) + xlab("Year") + ylab("Number of Passengers") + 
    ggtitle("Double Exponential Smoothing")

ggplot(AirPassengers, aes(x = seq(1949, 1960, 1/13), y = matrix(AirPassengers))) + 
    geom_line(lwd = 1.1, col = "#56B4E9") + geom_line(data = AirPassengers, aes(x = seq(1949, 1960, 
    1/13), y = c(rep(NA, 12), matrix(fitted(HW.out2)[, 1]))), col = "#000000", linetype = "dashed", 
    lwd = 1.1) + theme(text = element_text(size = 20)) + xlab("Year") + ylab("Number of Passengers") + 
    ggtitle("Holt-Winter (Additive)")

ggplot(AirPassengers, aes(x = seq(1949, 1960, 1/13), y = matrix(AirPassengers))) + 
    geom_line(lwd = 1.1, col = "#56B4E9") + geom_line(data = AirPassengers, aes(x = seq(1949, 1960, 
    1/13), y = c(rep(NA, 12), matrix(fitted(HW.out1)[, 1]))), col = "#000000", linetype = "dashed", 
    lwd = 1.1) + theme(text = element_text(size = 20)) + xlab("Year") + ylab("Number of Passengers") + 
    ggtitle("Holt-Winter (Multiplicative)")

ARIMA.out <- arima(AirPassengers, order = c(1, 1, 1))
ARIMA.out
predict(ARIMA.out, 6)

