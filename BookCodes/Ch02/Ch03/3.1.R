library(ggplot2)
library(HoRM)
library(aprean3)
library(tolerance)
library(psych)
library(investr)

###############################################################################
### Example 3.6.1: Steam Output Data, cont'd
###############################################################################

data(dsa01a, package = "aprean3")

temp <- dsa01a$x8
steam <- dsa01a$x1
out <- lm(steam ~ temp)
summary(out)

power.b(temp, steam, 0.05)
power.b(temp, steam, 0.05, B0 = 13.6, B1 = -0.05)

# 95% Confidence Interval for Slope
out$coef[2] + c(-1, 1) * qt(0.975, 23) * summary(out)$coef[2, 2]

# Bonferroni CIs
out$coef[1] + c(-1, 1) * qt(1 - 0.05/4, 23) * summary(out)$coef[1, 2]
out$coef[2] + c(-1, 1) * qt(1 - 0.05/4, 23) * summary(out)$coef[2, 2]

MSE <- anova(out)$"Mean Sq"[2]
n <- length(temp)
x.new <- c(40, 50)
y.hat <- predict(out, newdata = data.frame(temp = x.new))
se.y.hat <- sqrt(MSE * ((1/n) + (x.new - mean(temp))^2/sum((temp - mean(temp))^2)))


# CI
predict(out, newdata = data.frame(temp = x.new), interval = "confidence")

# Bonferroni CI
y.hat[1] + c(-1, 1) * qt(1 - 0.05/(2 * 2), 23) * se.y.hat[1]
y.hat[2] + c(-1, 1) * qt(1 - 0.05/(2 * 2), 23) * se.y.hat[2]

# Working-Hotelling
y.hat[1] + c(-1, 1) * se.y.hat[1] * sqrt(2 * qf(1 - 0.05, 2, 23))
y.hat[2] + c(-1, 1) * se.y.hat[2] * sqrt(2 * qf(1 - 0.05, 2, 23))

# PI
predict(out, newdata = data.frame(temp = x.new), interval = "prediction")

# Bonferroni PI
y.hat[1] + c(-1, 1) * qt(1 - 0.05/(2 * 2), 23) * sqrt(MSE + (se.y.hat[1])^2)
y.hat[2] + c(-1, 1) * qt(1 - 0.05/(2 * 2), 23) * sqrt(MSE + (se.y.hat[2])^2)

# Scheffe PI
y.hat[1] + c(-1, 1) * sqrt(2 * qf(1 - 0.05, 2, 23) * (MSE + (se.y.hat[1])^2))
y.hat[2] + c(-1, 1) * sqrt(2 * qf(1 - 0.05, 2, 23) * (MSE + (se.y.hat[2])^2))

# One-Sided TIs
regtol.int(out, new.x = cbind(c(40, 50)), alpha = 0.05, P = 0.99, side = 1)[c(19, 
    14), ]

# Two-Sided TIs
regtol.int(out, new.x = cbind(c(40, 50)), alpha = 0.05, P = 0.99, side = 2)[c(19, 
    14), ]

# plotting CI's
CIs <- predict(out, newdata = data.frame(temp = temp[1:25]), interval = "confidence")
PIs <- predict(out, newdata = data.frame(temp = temp[1:25]), interval = "prediction")

new.y.hat <- predict(out)
new.se.y.hat <- sqrt(MSE * ((1/n) + (temp - mean(temp))^2/sum((temp - mean(temp))^2)))
lowers <- new.y.hat - new.se.y.hat * sqrt(2 * qf(1 - 0.05, 2, 23))
uppers <- new.y.hat + new.se.y.hat * sqrt(2 * qf(1 - 0.05, 2, 23))

twosided <- regtol.int(out, alpha = 0.05, P = 0.99, side = 2)[25:1, ]
frame <- data.frame(steam, temp)
newframe <- data.frame(frame, CIs[, 2], CIs[, 3], PIs[, 2], PIs[, 3], lowers, uppers, 
    twosided[, 5], twosided[, 6])
names(newframe) <- c("steam", "temp", "CI1", "CI2", "PI1", "PI2", "WH1", "WH2", "TS1", 
    "TS2")

ggplot(newframe, aes(x = temp, y = steam)) + geom_point(size = 3) + geom_line(aes(y = fitted.values(out), 
    color = "OLS", linetype = "OLS"), size = 1.3) + ggtitle("Steam Output Data") + 
    theme(text = element_text(size = 20)) + xlab("Temp") + ylab("Steam") + geom_line(aes(y = CI1, 
    color = "95% CI", linetype = "95% CI"), size = 1.3) + geom_line(aes(y = CI2, 
    color = "95% CI", linetype = "95% CI"), size = 1.3) + geom_line(aes(y = PI1, 
    color = "95% PI", linetype = "95% PI"), size = 1.3) + geom_line(aes(y = PI2, 
    color = "95% PI", linetype = "95% PI"), size = 1.3) + geom_line(aes(y = WH1, 
    color = "95% W-H bands", linetype = "95% W-H bands"), size = 1.3) + geom_line(aes(y = WH2, 
    color = "95% W-H bands", linetype = "95% W-H bands"), size = 1.3) + geom_line(aes(x = sort(temp), 
    y = TS1, color = "95%/99% two-sided\ntolerance intervals", linetype = "95%/99% two-sided\ntolerance intervals"), 
    size = 1.3) + geom_line(aes(x = sort(temp), y = TS2, color = "95%/99% two-sided\ntolerance intervals", 
    linetype = "95%/99% two-sided\ntolerance intervals"), size = 1.3) + scale_colour_manual(name = "Analysis Type", 
    values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "blue")) + scale_linetype_manual(name = "Analysis Type", 
    values = c("dashed", "dotted", "longdash", "dotdash", "solid")) + theme(legend.text = element_text(size = 12), 
    legend.title = element_text(size = 12), plot.title = element_text(size = 20), 
    axis.text = element_text(size = 20), axis.title.x = element_text(size = 20), 
    axis.title.y = element_text(size = 20)) + theme(legend.background = element_rect(color = "black", 
    size = 0.1)) + theme(legend.key.width = unit(3, "line"))

