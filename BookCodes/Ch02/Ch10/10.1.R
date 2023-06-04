library(ggplot2)
library(sandwich)
library(fastR)
library(car)
library(MASS)
library(strucchange)
library(outliers)
library(influence.ME)
library(climtrends)
library(faraway)
library(het.test)

###############################################################################
### Example 10.5.1: Punting Data
###############################################################################

data(punting, package = "faraway")
out <- lm(Hang ~ . - Distance, data = punting)
summary(out)

ggplot(punting, aes(fitted.values(out), resid(out))) + geom_point(size = 3) + theme(text = element_text(size = 20)) + 
    xlab("Fitted Values") + ylab("Raw Residuals") + geom_hline(yintercept = 0, color = 1, 
    size = 0.6) + ggtitle("Raw Residuals vs. Fitted Values")

ggplot(punting, aes(fitted.values(out), rstandard(out))) + geom_point(size = 3) + 
    theme(text = element_text(size = 20)) + xlab("Fitted Values") + ylab("Standardized Residuals") + 
    geom_hline(yintercept = 0, color = 1, size = 0.6) + ggtitle("Standardized Residuals vs. Fitted Values")

ggplot(punting, aes(fitted.values(out), studres(out))) + geom_point(size = 3) + theme(text = element_text(size = 20)) + 
    xlab("Fitted Values") + ylab("Studentized Residuals") + geom_hline(yintercept = 0, 
    color = 1, size = 0.6) + ggtitle("Studentized Residuals vs. Fitted Values")

# QQ-plot
ggplot(punting, aes(sample = studres(out))) + stat_qq(size = 3) + geom_abline(intercept = 0, 
    slope = 1, color = 4) + theme(text = element_text(size = 20)) + xlab("Theoretical Quantiles") + 
    ylab("Sample Quantiles") + ggtitle("Normal Q-Q Plot")

# Cook's distance
measures <- influence.measures(out)
ggplot(punting, aes(seq(1:13), measures$infmat[, 9])) + geom_point(size = 3) + geom_hline(yintercept = 0, 
    color = 1, size = 0.6) + theme(text = element_text(size = 20)) + xlab("Observation number") + 
    ylab("Cook's Distance") + ggtitle("Cook's Distance")

# DFITS
ggplot(punting, aes(seq(1:13), measures$infmat[, 7])) + geom_point(size = 3) + geom_hline(yintercept = 0, 
    color = 1, size = 0.6) + theme(text = element_text(size = 20)) + xlab("Observation number") + 
    ylab("DFITS") + ggtitle("DFITS")

diag.table <- round(data.frame(obser.repons = punting$Hang, fitted.rep = fitted.values(out), 
    leverage = measures$infmat[, 10], res = resid(out), stdres = rstandard(out), 
    studres = studres(out), DFIT = measures$infmat[, 7], Cookdistance = measures$infmat[, 
        9], CovRatio = measures$infmat[, 8]), 2)

diag.table

which.max(resid(out))
dixon.test(resid(out), opposite = F)  #maximum
which.min(resid(out))
dixon.test(resid(out), opposite = T)  #minimum

