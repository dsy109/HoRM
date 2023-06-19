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
### Example 13.5.1: Google Stock Data
###############################################################################

data(stock, package = "HoRM")

autoplot(stock, main = "Google Stock Prices (2005)") + labs(y = "Closing Price") + 
    theme(text = element_text(size = 20))
ggAcf(stock, main = "Google Stock Prices (2005)") + theme(text = element_text(size = 20))
ggPacf(stock, main = "Google Stock Prices (2005)") + theme(text = element_text(size = 20))

out1 <- acf(stock, plot = FALSE)
out1
out2 <- pacf(stock, plot = FALSE)
out2

Box.test(stock, lag = 1, type = "Ljung-Box")
Box.test(stock, lag = 1, type = "Box-Pierce")

stock.ar1 <- lm(stock[-1, ] ~ lag(stock, 1)[-1, ])
stock2 <- cbind(stock, c(stock[1, ], stock.ar1$fitted.value))
colnames(stock2) <- c("Data", "AR(1)")
autoplot(stock2, facet = NULL, main = "Google Stock Prices (2005)") + labs(y = "Closing Price") + 
    theme(text = element_text(size = 20))

