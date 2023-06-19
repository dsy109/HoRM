library(ggplot2)
library(HoRM)
library(conjoint)
library(aprean3)
library(car)
library(MASS)

###############################################################################
### Example 9.6.2: Steam Output Data, cont'd
###############################################################################

data(dsa01a, package = "aprean3")

ggplot(dsa01a, aes(x = x8, y = x1)) + geom_point(size = 3) + geom_smooth(method = lm) + 
    ggtitle("Steam Output Data (Uncoded)") + theme(text = element_text(size = 20)) + 
    xlab(expression(paste("Temperature (", degree, "F)", sep = ""))) + ylab("Steam Usage (Monthly)")
temp <- dsa01a$x8
steam <- dsa01a$x1
out <- lm(steam ~ temp)
summary(out)

new.df <- data.frame(steam, temp = round(temp, -1))
ggplot(new.df, aes(x = temp, y = steam)) + geom_point(size = 3) + geom_smooth(method = lm) + 
    ggtitle("Steam Output Data (Coded)") + theme(text = element_text(size = 20)) + 
    xlab(expression(paste("Coded Temperature (", degree, "F)", sep = ""))) + ylab("Steam Usage (Monthly)")
out2 <- lm(steam ~ temp, data = new.df)
summary(out2)

