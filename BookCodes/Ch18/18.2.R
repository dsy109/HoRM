library(ggplot2)
library(rgl)
library(AER)
library(survival)
library(truncreg)
library(SMPracticals)
library(KMsurv)

############################################################################### 
### Example 18.7.2: Motorette Data
############################################################################### 

data(motorette, package = "SMPracticals")

df1 <- data.frame(logtem = log(motorette$x), failuretime = motorette$y, Condition = motorette$cens)
df1$Condition <- ordered(df1$Condition, levels = 0:1, labels = c("Censored", "Uncensored"))
ggplot(df1, aes(x = logtem, y = failuretime, col = Condition, shape = Condition)) + geom_point(size = 3) +
    ggtitle("Motorette Data") + xlab("log(Temperature)") + ylab("Failure Time (hrs.)") + 
    theme(text = element_text(size = 20))

out.wei <- survreg(Surv(y, cens) ~ log(x), dist = "weibull", data = motorette)
out.loglogistic <- survreg(Surv(y, cens) ~ log(x), dist = "loglogistic", data = motorette)
out.lognorm <- survreg(Surv(y, cens) ~ log(x), dist = "lognormal", data = motorette)
out.exp <- survreg(Surv(y, cens) ~ log(x), dist = "exponential", data = motorette)
out.norm <- survreg(Surv(y, cens) ~ log(x), dist = "gaussian", data = motorette)
out.logistic <- survreg(Surv(y, cens) ~ log(x), dist = "logistic", data = motorette)

AIC.out <- cbind(c(AIC(out.wei), AIC(out.loglogistic), AIC(out.lognorm), AIC(out.exp), 
    AIC(out.norm), AIC(out.logistic)))
log.out <- cbind(c(as.numeric(logLik(out.wei)), as.numeric(logLik(out.loglogistic)), 
    as.numeric(logLik(out.lognorm)), as.numeric(logLik(out.exp)), as.numeric(logLik(out.norm)), 
    as.numeric(logLik(out.logistic))))
AIC.out <- cbind(AIC.out, log.out)
rownames(AIC.out) <- c("Weibull", "LogLogistic", "LogNormal", "Exponential", "Normal", 
    "Logistic")
colnames(AIC.out) <- c("AIC", "LogLike")
AIC.out

summary(out.wei)

anova(out.wei)

res <- residuals(out.wei, type = "deviance")
df2 <- data.frame(fittedvalues = fitted(out.wei), res = res)
ggplot(df2, aes(x = fittedvalues, y = res)) + geom_point(size = 3) + ggtitle("Deviance Residuals") + 
    xlab("Fitted Values") + ylab("Deviance Residuals") + geom_hline(yintercept = 0) + 
    theme(text = element_text(size = 20))

ggplot(df2, aes(sample = res)) + stat_qq(size = 3) + geom_abline(slope = 1, intercept = 0) + 
    ggtitle("Normal Q-Q Plot") + xlab("Theoretical Quantiles") + ylab("Sample Quantiles") + 
    theme(text = element_text(size = 20))

