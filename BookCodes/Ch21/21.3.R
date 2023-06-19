library(ggplot2)
library(HoRM)
library(betareg)
library(HSAUR2)
library(gee)
library(MuMIn)

###############################################################################
### Example 21.6.3: Credit Loss Data
###############################################################################

data(credloss, package = "HoRM")

out <- betareg(I(PD/100) ~ year + defs + LGD.mean + LGD.vol, data = credloss)
summary(out)

out2 <- betareg(I(PD/100) ~ year + defs, data = credloss)
summary(out2)

BIC(out)
BIC(out2)

defs.new <- seq(0, 200, length = 1000)
new.1985 <- predict(out2, newdata = data.frame(year = rep(1985, 1000), defs = defs.new)) * 
    100
new.1995 <- predict(out2, newdata = data.frame(year = rep(1995, 1000), defs = defs.new)) * 
    100
new.2005 <- predict(out2, newdata = data.frame(year = rep(2005, 1000), defs = defs.new)) * 
    100

DF1 <- data.frame(newdefs = defs.new, new1985 = new.1985, new1995 = new.1995, new2005 = new.2005)
ggplot(data = credloss, aes(x = defs, y = PD)) + geom_point(size = 3) + ggtitle("Credit Loss Data") + 
    xlab("Number of Defaults") + ylab("Probability of Default") + geom_line(data = DF1, 
    aes(x = newdefs, y = new1985, linetype = "1985", col = "1985")) + geom_line(data = DF1, 
    aes(x = newdefs, y = new1995, linetype = "1995", col = "1995")) + geom_line(data = DF1, 
    aes(x = newdefs, y = new2005, linetype = "2005", col = "2005")) + scale_color_manual("Year", 
    values = c("red", "black", "blue")) + scale_linetype_manual("Year", values = c("solid", 
    "dashed", "dotted")) + theme(text = element_text(size = 20))

DF2 <- data.frame(fitted = out$fitted.values, res1 = residuals(out, type = "deviance"), 
    res2 = residuals(out, type = "pearson"), cd = cooks.distance(out), index = seq(1, 
        24, 1))
ggplot(data = DF2, aes(x = fitted, y = res1)) + geom_point(size = 3) + xlab("Fitted Values") + 
    ggtitle("Deviance Residuals") + ylab("Deviance Residuals") + geom_abline(intercept = 0, 
    slope = 0) + theme(text = element_text(size = 20))

ggplot(data = DF2, aes(x = fitted, y = res2)) + geom_point(size = 3) + xlab("Fitted Values") + 
    ggtitle("Pearson Residuals") + ylab("Pearson Residuals") + geom_abline(intercept = 0, 
    slope = 0) + theme(text = element_text(size = 20))

ggplot(data = DF2, aes(x = index, y = cd)) + geom_bar(stat = "identity", position = "dodge", 
    width = 0.1) + ggtitle("Cook's Distance") + ylab("Cook's Distance") + theme(text = element_text(size = 20)) + 
    xlab("Index")

