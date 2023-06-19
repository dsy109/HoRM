library(ggplot2)
library(HoRM)
library(astrodatR)
library(np)
library(MASS)
library(deming)
library(kknn)
library(waveslim)
library(orthopolynom)
library(SemiPar)
library(mosaic)
library(L1pack)

###############################################################################
### Example 17.5.1: Gamma-Ray Burst Data
###############################################################################

data(GRB, package = "HoRM")
attach(GRB)

grb <- data.frame(GRB, lTIME = log(TIME), lFLUX = log(FLUX), lTIME.7 = (log(TIME) - 
    7) * (log(TIME) > 7), lTIME.8.5 = (log(TIME) - 8.5) * (log(TIME) > 8.5))
out1 <- lm(lFLUX ~ lTIME, data = grb)
out2 <- lm(lFLUX ~ lTIME + lTIME.7, data = grb)
out3 <- lm(lFLUX ~ lTIME + lTIME.8.5, data = grb)
BIC.out <- cbind(BIC(out1), BIC(out2), BIC(out3))
colnames(BIC.out) <- c("Model.1", "Model.2", "Model.3")
rownames(BIC.out) <- "BIC"
BIC.out

df5 <- data.frame(x = grb$TIME, y = grb$FLUX, xlog = log(grb$TIME), ylog = log(grb$FLUX))
ggplot(df5, aes(x = grb$TIME, y = grb$FLUX)) + geom_point(size = 3) + xlab("Time") + 
    ylab("Flux") + ggtitle("Gamma-Ray Burst Data") + theme(text = element_text(size = 20))

X <- seq(4, 14, len = 63)
ggplot(df5, aes(x = xlog, y = ylog)) + geom_point(size = 3) + xlab("log Time") + 
    ylab("log Flux") + ggtitle("Gamma-Ray Burst Data") + geom_line(aes(x = X, y = cbind(1, 
    X, (X - 7) * (X > 7)) %*% cbind(coef(out2)))) + geom_vline(xintercept = 7, 
    linetype = 2) + theme(text = element_text(size = 20))

summary(out2)

