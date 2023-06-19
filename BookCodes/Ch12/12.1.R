library(ggplot2)
library(HoRM)
library(deming)
library(robustbase)
library(car)
library(MASS)
library(RFreak)
library(L1pack)
library(fda)
library(bootstrap)
library(boot)
library(iterpc)

###############################################################################
### Example 12.6.1: Computer-Assisted Learning Data
###############################################################################

data(compasst, package = "HoRM")
attach(compasst)

out <- lm(cost ~ num.responses, data = compasst)
summary(out)
res <- out$res
a.res <- abs(res)

out2 <- lm(a.res ~ num.responses)
w <- 1/sqrt(out2$fit)
w[3] <- w[3]/2
out3 <- lm(cost ~ num.responses, weights = w)

ggplot(compasst, aes(x = num.responses, y = cost)) + geom_point(size = 3) + geom_smooth(method = lm, 
    se = FALSE) + ggtitle("Computer-Assisted Learning Data") + theme(text = element_text(size = 20)) + 
    xlab("Number of Responses") + ylab("Cost of Computer Time")

ggplot(compasst, aes(x = fitted.values(out), y = studres(out))) + geom_point(size = 3) + 
    ggtitle("Studentized Residuals") + theme(text = element_text(size = 20)) + xlab("Fitted Values") + 
    ylab("Studentized Residuals") + geom_hline(yintercept = 0, color = 1, size = 0.6)

ggplot(compasst, aes(x = num.responses, y = a.res)) + geom_point(size = 3) + ggtitle("Absolute Residuals") + 
    theme(text = element_text(size = 20)) + xlab("Predictor") + ylab("Absolute Residuals")

ggplot(compasst, aes(x = num.responses, y = cost)) + geom_point(size = 3) + ggtitle("Computer-Assisted Learning Data") + 
    xlab("Number of Responses") + ylab("Cost of Computer Time") + geom_abline(aes(slope = out$coefficients[2], 
    intercept = out$coefficients[1], color = "OLS", linetype = "OLS"), size = 1.3) + 
    geom_abline(aes(slope = out3$coefficients[2], intercept = out3$coefficients[1], 
        color = "WLS", linetype = "WLS"), size = 1.3) + scale_colour_manual(name = "Regression Type", 
    values = c("#E69F00", "#56B4E9")) + scale_linetype_manual(name = "Regression Type", 
    values = c("dashed", "dotted")) + theme(legend.text = element_text(size = 10), 
    legend.title = element_text(size = 10), plot.title = element_text(size = 25), 
    axis.text = element_text(size = 20), axis.title.x = element_text(size = 20), 
    axis.title.y = element_text(size = 20)) + theme(legend.background = element_rect(color = "black", 
    size = 0.1)) + theme(legend.key.width = unit(3, "line"))

ggplot(compasst, aes(x = fitted.values(out3), y = studres(out3))) + geom_point(size = 3) + 
    ggtitle("Studentized Residuals (WLS Fit)") + theme(text = element_text(size = 20)) + 
    xlab("Fitted Values of WLS ") + ylab("Studentized Residuals") + geom_hline(yintercept = 0, 
    color = 1, size = 0.6)

