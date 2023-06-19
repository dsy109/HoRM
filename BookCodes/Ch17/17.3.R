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
### Example 17.5.3: LIDAR Data
###############################################################################

data(lidar, package = "SemiPar")

ggplot(lidar, aes(range, logratio)) + geom_point() + ggtitle("LIDAR Data") + xlab("range") + 
    ylab("logratio") + theme(text = element_text(size = 20))

regressogram(lidar$range, lidar$logratio, nbins = 10, x.lab = "range", y.lab = "logratio", 
    main = "Regressogram (10 Bins)")

k <- 49
n.grid <- 100
range.grid <- seq(min(lidar$range), max(lidar$range), length = n.grid)
test.grid <- data.frame(range = range.grid, logratio = rep(0, n.grid))

df <- data.frame(x = lidar$range, y = lidar$logratio)
mh <- kknn(logratio ~ range, lidar, test.grid, k = k, kernel = "rectangular")
mh2 <- kknn(logratio ~ range, lidar, test.grid, k = k, kernel = "biweight")
df1 <- data.frame(x = c(range.grid, range.grid), y = c(fitted(mh), fitted(mh2)), 
    Kernel = c(rep("Rectangular", 100), rep("Biweight", 100)))
ggplot(data = df, aes(x = x, y = y)) + geom_point() + ggtitle("LIDAR Data (NN Regression)") + 
    theme(text = element_text(size = 20)) + xlab("range") + ylab("logratio") + geom_line(data = df1, 
    aes(x = x, y = y, col = Kernel, linetype = Kernel, lwd = 1.2), lwd = 1.2)

mk <- runmed(lidar$logratio, k, endrule = "median")
df2 <- data.frame(x = lidar$range, y = mk)
ggplot(data = df, aes(x, y)) + geom_point() + geom_line(data = df2, aes(x, y), col = "red", 
    lwd = 1.2) + ggtitle("LIDAR Data (Median Smoothing Regression)") + xlab("range") + 
    ylab("logratio") + theme(text = element_text(size = 20))

px <- legendre.polynomials(n = 5, normalized = FALSE)
lx <- poly2form(px, lidar$range)
b <- lm(lidar$logratio ~ lx - 1)$coefficients
mo <- cbind(lidar$range, (lx %*% b))
df3 <- data.frame(x = mo[, 1], y = mo[, 2])
ggplot(data = df, aes(x, y)) + geom_point() + geom_line(data = df3, aes(x, y), col = "red", 
    lwd = 1.2) + ggtitle("LIDAR Data (Orthogonal Series Regression)") + xlab("range") + 
    ylab("logratio") + theme(text = element_text(size = 20))

xs <- seq(1, nrow(lidar), length = 128)
lo <- floor(xs)
hi <- ceiling(xs)
y.int <- (xs - lo) * lidar$logratio[hi] + (hi - xs) * lidar$logratio[lo] + lidar$logratio[lo] * 
    (lo == hi)
yw <- dwt(y.int, wf = "d8", 4)
st <- idwt(manual.thresh(yw, 4, 5, hard = FALSE))
ht <- idwt(manual.thresh(yw, 4, 5, hard = TRUE))
df4 <- data.frame(x = c(lidar$range[xs], lidar$range[xs]), y = c(st, ht), Thresholding = c(rep("Soft", 
    128), rep("Hard", 128)))
ggplot(data = df, aes(x, y)) + geom_point() + ggtitle("LIDAR Data (Wavelet Regression)") + 
    theme(text = element_text(size = 20)) + xlab("range") + ylab("logratio") + geom_line(data = df4, 
    aes(x = x, y = y, col = Thresholding, linetype = Thresholding), lwd = 1.2)
