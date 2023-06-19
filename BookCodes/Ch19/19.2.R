library(ggplot2)
library(HoRM)
library(nlstools)

###############################################################################
### Example 19.4.2: Light Data
###############################################################################

data(light, package = "HoRM")
orl.nl2 <- nls(reading ~ b0 + b1 * exp(b2 * concentration), start = c(b0 = 0, b1 = 2, 
    b2 = -0.5), data = light)
summary(orl.nl2)

new.x <- seq(0, 5, len = 1000)
df2 <- data.frame(concentration = new.x)
df2$reading <- predict(orl.nl2, list(concentration = new.x))

ggplot(data = light, aes(x = concentration, y = reading)) + geom_point(size = 3) + 
    geom_line(data = df2, aes(x = concentration, y = reading), col = 2) + ggtitle("Light Experiment Data") + 
    theme(text = element_text(size = 20)) + xlab("Concentration (mg/L)") + ylab("Optical Reading")

res <- data.frame(x = fitted.values(orl.nl2), y = resid(orl.nl2))
ggplot(res, aes(x, y)) + geom_point(size = 3) + theme(text = element_text(size = 20)) + 
    xlab("Fitted Values") + ylab("Residuals") + geom_hline(yintercept = 0, color = 1, 
    size = 0.6) + ggtitle("Raw Residuals")

