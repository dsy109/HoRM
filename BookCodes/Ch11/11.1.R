library(ggplot2)
library(HoRM)
library(MethComp)
library(deming)
library(sem)

###############################################################################
### Example 11.6.1: Blood Alcohol Concentration Data
###############################################################################

data(BAC, package = "HoRM")
attach(BAC)

temp <- princomp(BAC)
P <- temp$loadings[, 2]

out <- lm(labtest ~ breath, data = BAC)
summary(out)
beta.OLS1 <- coef(out)

out2 <- lm(breath ~ labtest, data = BAC)
summary(out2)
b <- coef(out2)
beta.OLS2 <- c(-b[1]/b[2], 1/b[2])

y.hat <- (P[1] * mean(breath) + P[2] * mean(labtest) - P[1] * breath)/P[2]
out3 <- lm(y.hat ~ breath)
beta.TLS <- coef(out3)
beta.TLS

ggplot(BAC, aes(x = breath, y = labtest)) + geom_point(size = 3) + ggtitle("Blood Alcohol Concentration Data") + 
    theme(text = element_text(size = 20)) + xlab("Breathalyzer") + ylab("Laboratory Estimate") + 
    geom_abline(aes(slope = beta.OLS1[2], intercept = beta.OLS1[1], col = "OLS1", 
        linetype = "OLS1"), size = 1.3) + geom_abline(aes(slope = beta.OLS2[2], intercept = beta.OLS2[1], 
    color = "OLS2", linetype = "OLS2"), size = 1.3) + geom_abline(aes(slope = beta.TLS[2], 
    intercept = beta.TLS[1], color = "TLS", linetype = "TLS"), size = 1.3) + scale_colour_manual(name = "Regression Type", 
    values = c("#E69F00", "#56B4E9", "#009E73")) + scale_linetype_manual(name = "Regression Type", 
    values = c("dashed", "dotted", "longdash")) + theme(legend.text = element_text(size = 10), 
    legend.title = element_text(size = 10), plot.title = element_text(size = 25), 
    axis.text = element_text(size = 20), axis.title.x = element_text(size = 20), 
    axis.title.y = element_text(size = 20)) + theme(legend.background = element_rect(color = "black", 
    size = 0.1)) + theme(legend.key.width = unit(3, "line"))

