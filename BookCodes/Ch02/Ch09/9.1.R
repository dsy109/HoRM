library(ggplot2)
library(HoRM)
library(conjoint)
library(aprean3)
library(car)
library(MASS)

###############################################################################
### Example 9.6.1: Auditory Discrimination Data
###############################################################################

data(auditory, package = "HoRM")

out.lm <- lm(gain ~ pre.test * Culture, data = auditory)
summary(out.lm)

BETAS <- coef(out.lm)

ggplot(auditory, aes(x = pre.test, y = gain, color = Culture, shape = Culture)) + 
    geom_point(size = 3) + ggtitle("Auditory Data") + theme(text = element_text(size = 20)) + 
    xlab("Pre-Test") + ylab("Gain") + geom_abline(intercept = sum(BETAS[c(1, 3)]), 
    slope = sum(BETAS[c(2, 4)]), size = 1.3, col = "#00CDCD") + geom_abline(intercept = BETAS[1], 
    slope = BETAS[2], size = 1.3, col = "#FF7256")

