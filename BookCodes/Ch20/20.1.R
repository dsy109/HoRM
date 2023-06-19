library(ggplot2)
library(HoRM)
library(pROC)
library(lmtest)
library(mlogit)
library(nnet)
library(foreign)
library(pscl)
library(msme)
library(gamlss.tr)
library(VGAM)
library(reshape)

###############################################################################
### Example 20.5.1: Subarachnoid Hemorrhage Data
###############################################################################

data(aSAH, package = "pROC")
out <- glm(outcome == "Good" ~ age + s100b + ndka + gender, data = aSAH, family = binomial)
out.int <- glm(outcome == "Good" ~ 1, data = aSAH, family = binomial)

total <- logdiag(out)

# (1) Raw Residuals vs. fitted.values
ggplot(total, aes(x = total$fit, y = total$r.i)) + geom_point(size = 3) + ggtitle("Raw Residuals vs. Fitted Values") + 
    xlab("Fitted Values") + ylab("Raw Residuals") + stat_smooth(method = "loess") + 
    theme(text = element_text(size = 20), axis.title = element_text(size = 18), plot.title = element_text(size = 18)) + 
    geom_hline(yintercept = 0)

# (2) Pearson Residuals vs. Fitted values
ggplot(total, aes(x = total$fit, y = total$p.i)) + geom_point(size = 3) + ggtitle("Pearson Residuals vs. Fitted Values") + 
    xlab("Fitted Values") + ylab("Pearson Residuals") + stat_smooth(method = "loess") + 
    theme(text = element_text(size = 20), axis.title = element_text(size = 18), plot.title = element_text(size = 18)) + 
    geom_hline(yintercept = 0)

# (3) Deviance Residuals vs. Fitted values
ggplot(total, aes(x = total$fit, y = total$d.i)) + geom_point(size = 3) + ggtitle("Deviance Residuals vs. Fitted Values") + 
    xlab("Fitted Values") + ylab("Deviance Residuals") + stat_smooth(method = "loess") + 
    theme(text = element_text(size = 20), axis.title = element_text(size = 18), plot.title = element_text(size = 18)) + 
    geom_hline(yintercept = 0)

# (4) Studentized Raw Residuals vs. Fitted values
ggplot(total, aes(x = total$fit, y = total$stud.r.i)) + geom_point(size = 3) + ggtitle("Studentized Raw Residuals vs. Fitted Values") + 
    xlab("Fitted Values") + ylab("Studentized Raw Residuals") + stat_smooth(method = "loess") + 
    theme(text = element_text(size = 20), axis.title = element_text(size = 18), plot.title = element_text(size = 18)) + 
    geom_hline(yintercept = 0)

# (5) Studentized Pearson residuals vs. Fitted values
ggplot(total, aes(x = total$fit, y = total$stud.p.i)) + geom_point(size = 3) + ggtitle("Studentized Pearson Residuals vs. Fitted Values") + 
    xlab("Fitted Values") + ylab("Studentized Pearson Residuals") + stat_smooth(method = "loess") + 
    theme(text = element_text(size = 20), axis.title = element_text(size = 18), plot.title = element_text(size = 18)) + 
    geom_hline(yintercept = 0)

# (6) Studentized Deviance residuals vs. Fitted values
ggplot(total, aes(x = total$fit, y = total$stud.d.i)) + geom_point(size = 3) + ggtitle("Studentized Deviance Residuals vs. Fitted Values") + 
    xlab("Fitted Values") + ylab("Studentized Deviance Residuals") + stat_smooth(method = "loess") + 
    theme(text = element_text(size = 20), axis.title = element_text(size = 18), plot.title = element_text(size = 18)) + 
    geom_hline(yintercept = 0)

# Wald Test (Individual Coefficients)
summary(out)

# Odds Ratios
suppressWarnings(exp(cbind(OR = coef(out), confint(out))))

# Diagnostics
out.diag <- logdiag(out)

# Overall GOF Tests
GOF.tests(out)

# Test Subset of Predictors
out2 <- glm(outcome == "Good" ~ s100b + ndka, data = aSAH, family = binomial)
lmtest::lrtest(out2, out)
lmtest::waldtest(out2, out)

# pseudo-R2
as.numeric(1 - (exp(logLik(out.int))/exp(logLik(out)))^(2/length(out$y)))

new.dat1 <- data.frame(gos6 = 5, gender = as.factor(sort(rep(c("Female", "Male"), 
    61))), age = rep(20:80, 2), wfns = 1, s100b = mean(aSAH$s100b), ndka = mean(aSAH$ndka))
y.new <- predict(out, newdata = new.dat1, type = "response")
aSAH.pred <- data.frame(new.dat1, Probability = y.new)
names(aSAH.pred)[2] <- "Gender"
ggplot(aSAH.pred, aes(x = age, y = Probability, color = Gender, lty = Gender)) + geom_line(lwd = 1.3) + 
    xlab("Age") + theme(text = element_text(size = 20)) + ggtitle("Predicted Probabilities")

