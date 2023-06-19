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
### Example 20.5.4: Biochemistry Publications Data
###############################################################################

data(bioChemists, package = "pscl")
poireg <- glm(art ~ ., data = bioChemists, family = poisson)
nbreg <- glm.nb(art ~ ., data = bioChemists)
poihurdle <- hurdle(art ~ ., data = bioChemists, dist = "poisson")
nbhurdle <- hurdle(art ~ ., data = bioChemists, dist = "negbin")
zipreg <- zeroinfl(art ~ . | ., data = bioChemists, dist = "poisson")
zinbreg <- zeroinfl(art ~ . | ., data = bioChemists, dist = "negbin")

summary(poireg)
summary(nbreg)
summary(poihurdle)
summary(nbhurdle)
summary(zipreg)
summary(zinbreg)

out1 <- data.frame(round(cbind(c(coef(poireg), NA), c(coef(nbreg), summary(nbreg)$theta)), 
    4))
names(out1) <- c("Poi.Reg", "NB.Reg")
rownames(out1)[7] <- "theta"

out2 <- data.frame(round(cbind(c(coef(poihurdle), NA), c(coef(nbhurdle), summary(nbhurdle)$theta), 
    c(coef(zipreg), NA), c(coef(zinbreg), summary(zinbreg)$theta)), 4))
names(out2) <- c("Poi.Hurdle", "NB.Hurdle", "ZIP.Reg", "ZINB.Reg")
rownames(out2)[13] <- "theta"

out1
out2

AICs <- c(AIC(poireg, k = log(915)), AIC(nbreg, k = log(915)), AIC(poihurdle, k = log(915)), 
    AIC(nbhurdle, k = log(915)), AIC(zipreg, k = log(915)), AIC(zinbreg, k = log(915)))
names(AICs) <- c("Poi.Reg", "NB.Reg", "Poi.Hurdle", "NB.Hurdle", "ZIP.Reg", "ZINB.Reg")
AICs[c(1, 3, 5, 2, 4, 6)]

poireg.res <- residuals(poireg, type = "pearson")
nbreg.res <- residuals(nbreg, type = "pearson")
poihurdle.res <- residuals(poihurdle, type = "pearson")
nbhurdle.res <- residuals(nbhurdle, type = "pearson")
zipreg.res <- residuals(zipreg, type = "pearson")
zinbreg.res <- residuals(zinbreg, type = "pearson")

poireg.fit <- poireg$fitted.values
nbreg.fit <- nbreg$fitted.values
poihurdle.fit <- poihurdle$fitted.values
nbhurdle.fit <- nbhurdle$fitted.values
zipreg.fit <- zipreg$fitted.values
zinbreg.fit <- zinbreg$fitted.values

all <- data.frame(r1 = poireg.res, r2 = nbreg.res, r3 = poihurdle.res, r4 = nbhurdle.res, 
    r5 = zipreg.res, r6 = zinbreg.res, f1 = poireg.fit, f2 = nbreg.fit, f3 = poihurdle.fit, 
    f4 = nbhurdle.fit, f5 = zipreg.fit, f6 = zinbreg.fit)

# histogram
ggplot(bioChemists, aes(art)) + geom_histogram(binwidth = 1, color = "black", fill = "skyblue") + 
    ggtitle("Histogram of Articles Written") + xlab("Number of Articles Written") + 
    ylab("Frequency") + theme(text = element_text(size = 20))

# Poisson regresson
ggplot(all, aes(x = f1, y = r1)) + geom_point(size = 3) + ggtitle("Poisson Regression") + 
    theme(text = element_text(size = 20)) + xlab("Fitted Values") + ylab("Pearson Residuals") + 
    geom_smooth(method = "loess") + geom_hline(yintercept = 0, linetype = 2)

# Negative binomial regresson
ggplot(all, aes(x = f2, y = r2)) + geom_point(size = 3) + ggtitle("Negative Binomial Regression") + 
    theme(text = element_text(size = 20)) + xlab("Fitted Values") + ylab("Pearson Residuals") + 
    geom_smooth(method = "loess") + geom_hline(yintercept = 0, linetype = 2)

# Poisson hurdle regresson
ggplot(all, aes(x = f3, y = r3)) + geom_point(size = 3) + ggtitle("Poisson Hurdle Regression") + 
    theme(text = element_text(size = 20)) + xlab("Fitted Values") + ylab("Pearson Residuals") + 
    geom_smooth(method = "loess") + geom_hline(yintercept = 0, linetype = 2)

# Negative binomial hurdle regresson
ggplot(all, aes(x = f4, y = r4)) + geom_point(size = 3) + ggtitle("Negative Binomial Hurdle Regression") + 
    theme(text = element_text(size = 20)) + xlab("Fitted Values") + ylab("Pearson Residuals") + 
    geom_smooth(method = "loess") + geom_hline(yintercept = 0, linetype = 2)

# Zero_inflated Poisson regresson
ggplot(all, aes(x = f5, y = r5)) + geom_point(size = 3) + ggtitle("ZI Poisson Regression") + 
    theme(text = element_text(size = 20)) + xlab("Fitted Values") + ylab("Pearson Residuals") + 
    geom_smooth(method = "loess") + geom_hline(yintercept = 0, linetype = 2)

# Zero-inflated Negative Binomial regresson
ggplot(all, aes(x = f6, y = r6)) + geom_point(size = 3) + ggtitle("ZI Negative Binomial Regression") + 
    theme(text = element_text(size = 20)) + xlab("Fitted Values") + ylab("Pearson Residuals") + 
    geom_smooth(method = "loess") + geom_hline(yintercept = 0, linetype = 2)

