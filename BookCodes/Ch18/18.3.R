library(ggplot2)
library(rgl)
library(AER)
library(survival)
library(truncreg)
library(SMPracticals)
library(KMsurv)

###############################################################################
### Example 18.7.3: Bone Marrow Transplant Data
###############################################################################

data(hodg, package = "KMsurv")
fit <- coxph(Surv(time, delta) ~ factor(dtype) + factor(gtype) + score + wtime, method = "breslow", 
    data = hodg)
fit.mart <- resid(fit, type = "martingale")
coxsnellres <- hodg$delta - fit.mart
fitres <- survfit(coxph(Surv(coxsnellres, hodg$delta) ~ 1, method = "breslow"), type = "aalen")

df3 <- data.frame(x = fitres$time, y = -log(fitres$surv))
ggplot(df3, aes(x = x, y = y)) + ggtitle("Bone Marrow Transplant Data") + geom_step() + 
    xlab("Cox-Snell Residuals") + ylab("Estimated Cumulative Hazard Function") + 
    geom_abline(slope = 1, intercept = 0, col = 2, linetype = 2, lwd = 1.1) + theme(text = element_text(size = 20))

df.a <- data.frame(x = hodg$wtime, y = fit.mart)
ggplot(df.a, aes(x, y)) + geom_point() + ggtitle("Bone Marrow Transplant Data") + 
    xlab("Waiting Time to Transplant (months)") + ylab("Martingale Residuals") + 
    geom_hline(yintercept = 0) + geom_line(aes(x = lowess(hodg$wtime, fit.mart)$x, 
    y = lowess(hodg$wtime, fit.mart)$y), col = "red") + theme(text = element_text(size = 20))

df.b <- data.frame(x = hodg$score, y = fit.mart)
ggplot(df.b, aes(x, y)) + geom_point() + ggtitle("Bone Marrow Transplant Data") + 
    xlab("Karnofsky Score") + ylab("Martingale Residuals") + theme(text = element_text(size = 20)) + 
    geom_hline(yintercept = 0) + geom_line(aes(x = lowess(hodg$score, fit.mart)$x, 
    y = lowess(hodg$score, fit.mart)$y), col = "red")

fit.score <- resid(fit, type = "score")
df.c <- data.frame(x = hodg$wtime, y = fit.score[, 4])
ggplot(df.c, aes(x, y)) + geom_point() + ggtitle("Bone Marrow Transplant Data") + 
    xlab("Waiting Time to Transplant (months)") + ylab("Score Residuals") + theme(text = element_text(size = 20)) + 
    geom_hline(yintercept = 0) + geom_line(aes(x = lowess(hodg$wtime, fit.score[, 
    4])$x, y = lowess(hodg$wtime, fit.score[, 4])$y), col = "red")

df.d <- data.frame(x = hodg$score, y = fit.score[, 3])
ggplot(df.d, aes(x, y)) + geom_point() + ggtitle("Bone Marrow Transplant Data") + 
    xlab("Karnofsky Score") + ylab("Score Residuals") + theme(text = element_text(size = 20)) + 
    geom_hline(yintercept = 0) + geom_line(aes(x = lowess(hodg$score, fit.score[, 
    3])$x, y = lowess(hodg$score, fit.score[, 3])$y), col = "red")

fit.sch <- resid(fit, type = "schoenfeld")

wtime.ord <- hodg$wtime[sort(which(hodg$delta == 1))]
score.ord <- hodg$score[sort(which(hodg$delta == 1))]

df.e <- data.frame(x = wtime.ord, y = fit.sch[, 4])
ggplot(df.e, aes(x, y)) + geom_point() + ggtitle("Bone Marrow Transplant Data") + 
    xlab("Waiting Time to Transplant (months)") + ylab("Schoenfeld Residuals") + 
    theme(text = element_text(size = 20)) + geom_hline(yintercept = 0) + geom_line(aes(x = lowess(wtime.ord, 
    fit.sch[, 4])$x, y = lowess(wtime.ord, fit.sch[, 4])$y), col = "red")

df.f <- data.frame(x = score.ord, y = fit.sch[, 3])
ggplot(df.f, aes(x, y)) + geom_point() + ggtitle("Bone Marrow Transplant Data") + 
    xlab("Karnofsky Score") + ylab("Schoenfeld Residuals") + theme(text = element_text(size = 20)) + 
    geom_hline(yintercept = 0) + geom_line(aes(x = lowess(score.ord, fit.sch[, 3])$x, 
    y = lowess(score.ord, fit.sch[, 3])$y), col = "red")

