library(ggplot2)
library(HoRM)
library(MASS)
library(MPV)
library(StatDA)
library(rgl)
library(tolerance)

###############################################################################
### Example 6.7.2: Kola Project Data
###############################################################################

data(chorizon, package = "StatDA")
attach(chorizon)

x <- chorizon[chorizon$LITO == 1, "Cr"]
z <- chorizon[chorizon$LITO == 1, "Cr_INAA"]
y <- chorizon[chorizon$LITO == 1, "Co"]
n <- length(x)
kola <- data.frame(Cr = x, Co = y, Cr_INAA = z)

open3d()
out <- lm(Cr_INAA ~ ., data = kola)
out.res <- out$res
out.fit <- out$fit
col.p <- rep(0, n)
for (i in 1:n)
{
    if (out.res[i] < 0) 
        col.p[i] <- 2 else col.p[i] <- 3
}
plot3d(x, y, z, size = 0.5, col = col.p, alpha = c(0.75), type = "s", box = F, xlab = "Cr", 
    ylab = "Co", zlab = "Cr_INAA")
x1 <- seq(min(x), max(x), length = 30)
y1 <- seq(min(y), max(y), length = 30)
f <- function(x, y, out) out[1] + out[2] * x + out[3] * y
z1 <- outer(x1, y1, f, out = out$coef)
material3d(col = "gray")
persp3d(x1, y1, z1, aspect = c(1, 1, 0.5), col = "lightblue", add = T, alpha = c(0.5))
for (i in 1:n)
{
    segments3d(rbind(c(x[i], y[i], z[i]), c(x[i], y[i], out$coef[1] + out$coef[2] * 
        x[i] + out$coef[3] * y[i])), col = col.p[i])
}

kola.fit <- data.frame(fit = out.fit, studres = studres(out))

ggplot(kola.fit, aes(x = fit, y = studres)) + geom_point(size = 3) + theme(text = element_text(size = 20)) + 
    xlab("Fitted Values") + ylab("Studentized Residuals") + geom_hline(yintercept = 0, 
    color = 1, size = 0.6) + ggtitle("Scatterplot of Studentized Residuals")

summary(out)

kola.ln <- data.frame(ln.Cr = log(x), ln.Co = log(y), ln.Cr_INAA = log(z))

open3d()
out.ln <- lm(ln.Cr_INAA ~ ., data = kola.ln)
out.ln.res <- out.ln$res
out.ln.fit <- out.ln$fit
col.p <- rep(0, n)
lx <- log(x)
ly <- log(y)
lz <- log(z)
for (i in 1:n)
{
    if (out.ln.res[i] < 0) 
        col.p[i] <- 2 else col.p[i] <- 3
}
plot3d(lx, ly, lz, size = 0.5, col = col.p, alpha = c(0.75), type = "s", box = F, xlab = "log(Cr)", 
    ylab = "log(Co)", zlab = "log(Cr_INAA)")
lx1 <- seq(min(lx), max(lx), length = 30)
ly1 <- seq(min(ly), max(ly), length = 30)
lz1 <- outer(lx1, ly1, f, out = out.ln$coef)
material3d(col = "gray")
persp3d(lx1, ly1, lz1, aspect = c(1, 1, 0.5), col = "lightblue", add = T, alpha = c(0.5))
for (i in 1:n)
{
    segments3d(rbind(c(lx[i], ly[i], lz[i]), c(lx[i], ly[i], out.ln$coef[1] + out.ln$coef[2] * 
        lx[i] + out.ln$coef[3] * ly[i])), col = col.p[i])
}

kola.fit.ln <- data.frame(fit = out.ln.fit, studres = studres(out.ln))

ggplot(kola.fit.ln, aes(x = fit, y = studres)) + geom_point(size = 3) + theme(text = element_text(size = 20)) + 
    xlab("Fitted Values") + ylab("Studentized Residuals") + geom_hline(yintercept = 0, 
    color = 1, size = 0.6) + ggtitle("Scatterplot of Studentized Residuals")

summary(out.ln)

out.slr <- lm(ln.Cr_INAA ~ ln.Cr, data = kola.ln)
kola.fit.slr <- data.frame(kola.ln, fit = out.slr$fit, studres = studres(out.slr))

ggplot(kola.fit.slr, aes(x = ln.Cr, y = ln.Cr_INAA)) + geom_point(size = 3) + geom_smooth(method = lm) + 
    theme(text = element_text(size = 20)) + xlab("log(Cr)") + ylab("log(Cr_INAA)") + 
    ggtitle("Transformed Kola Project Data")

ggplot(kola.fit.slr, aes(x = fit, y = studres)) + geom_point(size = 3) + theme(text = element_text(size = 20)) + 
    xlab("Fitted Values") + ylab("Studentized Residuals") + geom_hline(yintercept = 0, 
    color = 1, size = 0.6) + ggtitle("Scatterplot of Studentized Residuals")

summary(out.slr)

