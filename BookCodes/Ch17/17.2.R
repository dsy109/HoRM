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
### Example 17.5.2: Quasar Data
###############################################################################

data(SDSS_QSO, package = "astrodatR")
quasars <- SDSS_QSO
quas <- quasars
quas[quas == 0 | quas == -1 | quas == -9] <- NA
quas <- na.omit(quas)
dim(quas)

r_i <- quasars$r_mag - quasars$i_mag
z <- quasars$z

df1 <- data.frame(z, r_i)
ggplot(data = df1, aes(x = z, y = r_i)) + ggtitle("Quasar Data") + xlab("Redshift") + 
    ylab("r-i") + theme(text = element_text(size = 20)) + geom_point(size = 0.5)

z1 <- sort(z)
r_i1 <- r_i[order(z)]
# LOESS Fit
fit1 <- loess(r_i1 ~ z1, span = 0.1)
# Smoothing Spline
fit2 <- smooth.spline(z1, r_i1, nknots = 100)
# Nadaraya-Watson Estimator
bw <- npregbw(r_i1 ~ z1, regtype = "lc", bwtype = "fixed", bandwidth.compute = F, 
    bws = 0.1)
fit3 <- npreg(bws = bw, gradients = FALSE)

r <- length(z1)
ash.df <- data.frame(x = rep(z1, 3), fits = c(predict(fit1), predict(fit2, x = z1)$y, 
    fitted(fit3)), Smoother = c(rep("LOESS", r), rep("Spline", r), rep("N-W", r)))
ggplot(df1, aes(x = z, y = r_i)) + stat_bin2d(bins = 200) + geom_line(data = ash.df, 
    aes(x = x, y = fits, col = Smoother, linetype = Smoother), lwd = 1) + ggtitle("Quasar Data") + 
    xlab("Redshift") + ylab("r-i") + theme(text = element_text(size = 20)) + scale_fill_gradient(low = "black", 
    high = "white", name = "Count")

summary(fit1)
RSE <- sqrt(sum((fit2$y - fit2$yin)^2)/(length(fit2$y) - fit2$df))
cat(c("Smoothing Parameter: spar=", round(fit2$spar, 5), "\n", "Equivalent Degrees of Freedom (Df): ", 
    round(fit2$df, 5), "\n", "Residual standard error: ", round(RSE, 5), "\n", "Penalized Criterion: ", 
    round(fit2$pen.crit, 5), "\n", "GCV: ", round(fit2$cv.crit, 5)), sep = "")
summary(fit3)

