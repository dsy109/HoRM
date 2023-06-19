library(ggplot2)
library(HoRM)
library(RColorBrewer)
library(lme4)
library(car)
library(rsm)
library(daewr)
library(mixexp)

###############################################################################
### Example 15.5.2: Cracker Promotion Data
###############################################################################

data(cracker, package = "HoRM")

out.lm <- lm(y ~ treat * x, data = cracker, contrasts = list(treat = contr.sum))
out.aov <- aov(y ~ x + treat, data = cracker, contrasts = list(treat = contr.sum))
Anova(out.lm, type = "III")
Anova(out.aov, type = "III")

lm.full <- lm(y ~ x + treat, data = cracker)
beta.hat <- coef(lm.full)[2]
MSE <- anova(lm.full)$"Mean Sq"[3]
x.bar <- mean(cracker$x)
x.i.bar <- as.numeric(by(cracker[, 4], cracker[, 1], mean))[c(2, 3, 1)]
y.i.bar <- as.numeric(by(cracker[, 3], cracker[, 1], mean))[c(2, 3, 1)]
y.i.adj <- y.i.bar - beta.hat * (x.i.bar - x.bar)

summary(lm.full)

Treatment <- c(rep("Treatment 1", 5), rep("Treatment 2", 5), rep("Treatment 3", 5))
cracker <- cbind(cracker, Treatment)

ggplot(cracker, aes(x = x, y = y, col = Treatment, shape = Treatment)) + geom_point(size = 3) + 
    xlab("Sales in Preceding Period") + ylab("Sales in Promotional Period") + ggtitle("Cracker Promotion Data") + 
    theme(text = element_text(size = 20))

ggplot(cracker, aes(x = x, y = y, col = Treatment, shape = Treatment)) + geom_point(size = 3) + 
    xlab("Sales in Preceding Period") + ylab("Sales in Promotional Period") + ggtitle("Cracker Promotion Data") + 
    geom_abline(slope = beta.hat, intercept = y.i.bar[1] + beta.hat * (-x.i.bar[1]), 
        col = "#EE6A50") + geom_abline(slope = beta.hat, intercept = y.i.bar[2] + 
    beta.hat * (-x.i.bar[2]), col = "#66CD00") + geom_abline(slope = beta.hat, intercept = y.i.bar[3] + 
    beta.hat * (-x.i.bar[3]), col = "#6495ED") + theme(text = element_text(size = 20))

