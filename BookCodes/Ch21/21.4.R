library(ggplot2)
library(HoRM)
library(betareg)
library(HSAUR2)
library(gee)
library(MuMIn)

###############################################################################
### Example 21.6.4: Epilepsy Data
###############################################################################

data(epilepsy, package = "HSAUR2")

options(digits = 3)

epilepsy$period <- as.numeric(epilepsy$period)

p <- qplot(period, seizure.rate/base, data = epilepsy[1:112, ]) + geom_point(size = 3) + 
    theme(text = element_text(size = 20)) + geom_smooth(method = "lm", formula = y ~ 
    x, se = FALSE, lty = 2)
p + facet_wrap(~subject, nrow = 4) + labs(title = "Epilepsy Data (Placebo)", x = "Period", 
    y = "Seizure Rate / Base")

p1 <- qplot(period, seizure.rate/base, data = epilepsy[113:236, ]) + geom_point(size = 3) + 
    theme(text = element_text(size = 20)) + geom_smooth(method = "lm", formula = y ~ 
    x, se = FALSE, lty = 2)
p1 + facet_wrap(~subject, nrow = 4) + labs(title = "Epilepsy Data (Progabide)", x = "Period", 
    y = "Seizure Rate / Base")

mean <- c(9.36, 8.58, 8.29, 8.42, 8.79, 8.13, 7.96, 6.71)
var <- c(102.8, 332.7, 66.7, 140.7, 215.3, 193, 58.2, 126.9)
Period <- c("Period 1", "Period 1", "Period 2", "Period 2", "Period 3", "Period 3", 
    "Period 4", "Period 4")
Treatment <- c("Placebo", "Progabide", "Placebo", "Progabide", "Placebo", "Progabide", 
    "Placebo", "Progabide")
meantable <- data.frame(mean, var, Period, Treatment)
ggplot(data = meantable, aes(x = mean, y = var, shape = Period, col = Treatment)) + 
    theme(text = element_text(size = 20)) + geom_point(size = 4) + xlab("Treatment by Time Period Means") + 
    ylab("Treatment by Time Period Variances") + ggtitle("Variances")

per <- rep(log(2), nrow(epilepsy))
glm_fit <- glm(seizure.rate ~ base + age + treatment, offset = per, data = epilepsy, 
    family = "quasipoisson")
gee_fit1 <- gee(seizure.rate ~ base + age + treatment + offset(per), data = epilepsy, 
    family = "poisson", id = subject, corstr = "independence", scale.fix = FALSE)  #Same as glm_fit
gee_fit2 <- gee(seizure.rate ~ base + age + treatment + offset(per), data = epilepsy, 
    family = "poisson", id = subject, corstr = "exchangeable", scale.fix = FALSE)
gee_fit3 <- gee(seizure.rate ~ base + age + treatment + offset(per), data = epilepsy, 
    family = "poisson", id = subject, corstr = "AR-M", Mv = 1, scale.fix = FALSE)

summary(glm_fit)
summary(gee_fit1)
summary(gee_fit2)
summary(gee_fit3)

QIC(gee_fit1, gee_fit2, gee_fit3)

