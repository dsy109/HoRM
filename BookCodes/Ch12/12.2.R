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
### Example 12.6.2: Arsenate Assay Data, cont'd
###############################################################################

data(arsenate, package = "deming")
attach(arsenate)

out.ls <- lm(aes ~ aas)
out.ls
confint(out.ls)
out.ts <- theilsen(aes ~ aas)
out.ts
out.pb <- pbreg(aes ~ aas)
out.pb

ggplot(arsenate, aes(x = fitted.values(out.ls), y = studres(out.ls))) + geom_point(size = 3) + 
    ggtitle("Studentized Residuals") + theme(text = element_text(size = 20)) + xlab("Fitted Values") + 
    ylab("Studentized Residuals") + geom_hline(yintercept = 0, color = 1, size = 0.6)

ggplot(arsenate, aes(x = seq(1, 30, 1), y = cooks.distance(out.ls))) + geom_point(size = 3) + 
    geom_hline(yintercept = 0, color = 1, size = 0.6) + theme(text = element_text(size = 20)) + 
    xlab("Observation number") + ylab("Cook's Distance") + ggtitle("Cook's Distance")

ggplot(arsenate, aes(x = aas, y = aes)) + geom_point(size = 3) + ggtitle("Arsenate Assay Data") + 
    theme(text = element_text(size = 20)) + xlab("Atomic Absorption Spectrometry (ug/L)") + 
    ylab("Atomic Emission Spectroscopy (ug/L)") + geom_abline(aes(slope = out.ls$coefficients[2], 
    intercept = out.ls$coefficients[1], color = "OLS", linetype = "OLS"), size = 1.3) + 
    geom_abline(aes(slope = out.ts$coefficients[2], intercept = out.ts$coefficients[1], 
        color = "Theil-Sen", linetype = "Theil-Sen"), size = 1.3) + geom_abline(aes(slope = out.pb$coefficients[2], 
    intercept = out.pb$coefficients[1], color = "Passing-Bablok", linetype = "Passing-Bablok"), 
    size = 1.3) + scale_colour_manual(name = "Regression Type", values = c("#E69F00", 
    "#56B4E9", "#009E73")) + scale_linetype_manual(name = "Regression Type", values = c("solid", 
    "dashed", "dotted")) + theme(legend.text = element_text(size = 10), legend.title = element_text(size = 10), 
    plot.title = element_text(size = 25), axis.text = element_text(size = 20), axis.title.x = element_text(size = 20), 
    axis.title.y = element_text(size = 20)) + theme(legend.background = element_rect(color = "black", 
    size = 0.1)) + theme(legend.key.width = unit(3, "line"))

n <- nrow(arsenate)

lad.out <- lad(aas ~ aes)  #L1 Regression
lad.out

win.out <- lmWinsor(aas ~ aes, data = arsenate, trim = 0.1)
win.out
suppressWarnings(summary(win.out)$sigma)

r1.out <- rlm(aas ~ aes, psi = psi.andrew)  #Andrews's Sine
summary(r1.out)

r2.out <- rlm(aas ~ aes, psi = psi.huber)  #Hampel's Method
summary(r2.out)

r3.out <- rlm(aas ~ aes, psi = psi.hampel, a = 1.5 * 0.902, b = 3.5 * 0.902, c = 8 * 
    0.902)  #Huber's Method
summary(r3.out)

r4.out <- rlm(aas ~ aes, psi = psi.bisquare)  #Tukey's Biweight
summary(r4.out)

lqs.out <- lqs(aes, aas, method = "lqs", quantile = 27)  #Least Quantile of Squares
lqs.out

lts.out <- lqs(aes, aas, method = "lts", quantile = 27)  #Least Trimmed Sum of Squares
lts.out

lta.out <- robreg.evol(aes, aas, method = "lta", quantile = 27)  #Least Trimmed Sum of Absolute Deviations
lta.out

mystat <- function(a, b)
{
    lad(a[b, 2] ~ a[b, 1])$coef
}
set.seed(10)
dat <- data.frame(aes, aas)
model.BS <- boot(dat, mystat, 1000)
mystat2 <- function(b, a, j)
{
    lad(a[b, 2] ~ a[b, 1])$coef[j]
}
model.JK.int <- jackknife(1:n, mystat2, dat, j = 1)
model.JK.slope <- jackknife(1:n, mystat2, dat, j = 2)
JK.out <- cbind(lad.out$coefficients, c(model.JK.int$jack.bias, model.JK.slope$jack.bias), 
    c(model.JK.int$jack.se, model.JK.slope$jack.se))
rownames(JK.out) <- c("j1*", "j2*")
colnames(JK.out) <- c("original", "bias", "std. error")
model.BS
JK.out

quantile(model.BS$t[, 1], c(0.05, 0.95), type = 1)
quantile(model.BS$t[, 2], c(0.05, 0.95), type = 1)
quantile(model.JK.int$jack.values, c(0.05, 0.95), type = 1)
quantile(model.JK.slope$jack.values, c(0.05, 0.95), type = 1)

