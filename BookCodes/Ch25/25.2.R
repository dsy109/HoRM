library(ggplot2)
library(MASS)
library(lattice)
library(HLMdiag)
library(fda)
library(rgl)
library(DepthProc)
library(alr3)
library(NPCirc)
library(quantreg)
library(mixtools)
library(Rfit)
library(extRemes)
library(ismev)
library(spdep)
library(multilevel)
library(survey)
library(SDaA)
library(bayesm)
library(coda)
library(ggmcmc)
library(SemiPar)
library(gcmr)
library(MAd)
library(metafor)
library(VIM)
library(astsa)
library(rTensor)
library(tensorA)
library(dplyr)
library(RandomFields)
library(lme4)

###############################################################################
### Example 25.2.1: Gait Data
###############################################################################

data(gait, package = "fda")

gait.time <- as.numeric(rownames(gait)) * 20
hip <- gait[, , 1]
knee <- gait[, , 2]

col.tmp <- rep(palette(), 5)[-40]
df1 <- data.frame(cbind(gait.time = rep(gait.time, 39), hip = as.numeric(hip), group = as.numeric(sapply(1:39, 
    function(i) rep(i, 20)))))
df1$group <- as.factor(df1$group)
ggplot(data = df1, aes(x = gait.time, y = hip, col = group)) + geom_line() + xlim(0, 
    20) + ylim(-15, 85) + ggtitle("Hip Angle") + xlab("Gait Time") + ylab("Hip Angle") + 
    scale_color_manual(guide = FALSE, values = col.tmp) + theme(text = element_text(size = 20))

df2 <- data.frame(cbind(gait.time = rep(gait.time, 39), knee = as.numeric(knee), 
    group = as.numeric(sapply(1:39, function(i) rep(i, 20)))))
df2$group <- as.factor(df1$group)
ggplot(data = df2, aes(x = gait.time, y = knee, col = group)) + geom_line() + xlim(0, 
    20) + ylim(-15, 85) + ggtitle("Knee Angle") + xlab("Gait Time") + ylab("Knee Angle") + 
    scale_color_manual(guide = FALSE, values = col.tmp) + theme(text = element_text(size = 20))

x <- sapply(1:39, function(i) gait.time)
col.x <- sapply(1:39, function(i) rep(i, 20))
plot3d(x, gait[, , 1], gait[, , 2], col = col.x, type = "l", xlab = "Gait Time", 
    ylab = "Hip Angle", zlab = "Knee Angle")

gait.range <- c(0, 20)
gaitbasis.1 <- create.fourier.basis(gait.range, nbasis = 21)
harmaccelLfd <- vec2Lfd(c(0, (2 * pi/20)^2, 0), rangeval = gait.range)
gait.1 <- smooth.basisPar(gait.time, gait, gaitbasis.1, Lfdobj = harmaccelLfd, lambda = 0.01)$fd

hip.fd <- gait.1[, 1]
knee.fd <- gait.1[, 2]

knee.hip.fd <- fRegress(knee.fd ~ hip.fd)
predict.knee <- predict(knee.hip.fd)

col.tmp <- rep(palette(), 5)[-40]
df3 <- data.frame(cbind(gait.time = rep(gait.time, 39), knee = as.numeric(gait[, 
    , 2]), group = as.numeric(sapply(1:39, function(i) rep(i, 20)))))
df4 <- data.frame(cbind(argvals = rep(predict.knee$argvals, 39), pred = as.vector(predict.knee$y), 
    group = as.numeric(sapply(1:39, function(i) rep(i, 501)))))
df3$group <- as.factor(df3$group)
df4$group <- as.factor(df4$group)
ggplot(data = df3, aes(x = gait.time, y = knee, group = group, col = group)) + geom_point() + 
    geom_line(data = df4, aes(x = argvals, y = pred, group = group, col = group)) + 
    xlim(0, 20) + ylim(-15, 85) + ggtitle("Predicted and Observed Knee Angle") + 
    xlab("Gait Time") + ylab("Knee Angle") + scale_color_manual(guide = FALSE, values = col.tmp) + 
    theme(text = element_text(size = 20))

