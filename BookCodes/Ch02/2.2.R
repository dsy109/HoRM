library(ggplot2)
library(HoRM)
library(aprean3)
library(DescTools)
library(Hmisc)

###############################################################################
### Example 2.8.2: Steam Output Data
###############################################################################

data(dsa01a, package = "aprean3")

ggplot(dsa01a, aes(x = x8, y = x1)) + geom_point(size = 3) + geom_smooth(method = lm, 
    se = FALSE) + ggtitle("Steam Output Data") + theme(text = element_text(size = 20)) + 
    xlab(expression(paste("Temperature (", degree, "F)", sep = ""))) + ylab("Steam Usage (Monthly)")

temp <- dsa01a$x8
steam <- dsa01a$x1
out <- lm(steam ~ temp)
summary(out)

sqrt(summary(out)$r.squared)
