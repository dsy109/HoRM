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
### Example 20.5.5: Hospital Stays Data
###############################################################################

data(medpar, package = "msme")
medpar$type <- as.factor(medpar$type)
medpar$white <- as.factor(medpar$white)
medpar$died <- as.factor(medpar$died)
medpar$los <- as.integer(medpar$los)
ztp <- vglm(los ~ white + died + type, data = medpar, family = pospoisson())
ztnb <- vglm(los ~ white + died + type, data = medpar, family = posnegbinomial())

ggplot(medpar, aes(white, los)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
    col = "red") + geom_jitter(size = 1.5) + scale_y_log10() + ggtitle("Violin Plot for Race") + 
    xlab("Race") + ylab("Length of Stay (Days)") + theme(text = element_text(size = 20))

ggplot(medpar, aes(type, los)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
    col = "red") + geom_jitter(size = 1.5) + scale_y_log10() + ggtitle("Violin Plot for Admittance Type") + 
    xlab("Admittance Type") + ylab("Length of Stay (Days)") + theme(text = element_text(size = 20))

ggplot(medpar, aes(died, los)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
    col = "red") + geom_jitter(size = 1.5) + scale_y_log10() + ggtitle("Violin Plot for Death Indicator") + 
    xlab("Died?") + ylab("Length of Stay (Days)") + theme(text = element_text(size = 20))

summary(ztp)
summary(ztnb)

ztp.res <- residuals(ztp, type = "pearson")
ztnb.res <- residuals(ztnb, type = "pearson")

ztp.fit <- fitted.values(ztp)
ztnb.fit <- fitted.values(ztnb)

BICs <- c(AIC(ztp, k = log(nrow(medpar))), AIC(ztnb, k = log(nrow(medpar))))
names(BICs) <- c("ZT.Poi.Reg", "ZT.NB.Reg")
BICs

df <- data.frame(res1 = ztp.res, res2 = ztnb.res[, 1], fit1 = ztp.fit, fit2 = ztnb.fit)

# zero-truncated Poisson
ggplot(df, aes(x = fit1, y = res1)) + geom_point(size = 3) + ggtitle("ZT Poisson Regression") + 
    theme(text = element_text(size = 20)) + xlab("Fitted Values") + ylab("Pearson Residuals") + 
    geom_smooth(method = "loess") + geom_hline(yintercept = 0, linetype = 2)

# zero-truncated negative binomial fits
ggplot(df, aes(x = fit2, y = res2)) + geom_point(size = 3) + ggtitle("ZT Negative Binomial Regression") + 
    theme(text = element_text(size = 20)) + xlab("Fitted Values") + ylab("Pearson Residuals") + 
    geom_smooth(method = "loess") + geom_hline(yintercept = 0, linetype = 2)
