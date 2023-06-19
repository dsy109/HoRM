library(gplm)
library(HoRM)
library(mgcv)
library(car)
library(visreg)
library(plyr)
library(ggplot2)
library(ElemStatLearn)
library(SemiPar)
library(np)
library(gridExtra)
library(grid)
library(MASS)
library(GGally)
library(sfsmisc)

###############################################################################
### Example 23.7.1: The 1907 Romanian Peasant Revolt Data
###############################################################################

data(Chirot, package = "car")
ggscatmat(Chirot) + theme(text = element_text(size = 11),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

out.lm <- lm(intensity ~ ., data = Chirot)
summary(out.lm)
var.names <- c(tradition = "Tradition", midpeasant = "Midpeasant", inequality = "Inequality", 
    commerce = "Commerce")

out.am <- gam(intensity ~ s(tradition, k = 3) + s(midpeasant, k = 3) + s(inequality, 
    k = 3) + s(commerce, k = 3), data = Chirot, fam = gaussian)
summary(out.am)

plot.am <- visreg(out.am, type = "contrast", plot = FALSE)
smooths <- ldply(plot.am, function(part) data.frame(Variable = part$meta$x, x = part$fit[[part$meta$x]], 
    smooth = part$fit$visregFit, lower = part$fit$visregLwr, upper = part$fit$visregUpr))

res.am <- ldply(plot.am, function(part) data.frame(Variable = part$meta$x, x = part$res[[part$meta$x]], 
    y = part$res$visregRes))

ggplot(smooths, aes(x, smooth)) + geom_line() + geom_line(aes(y = lower), linetype = "dashed") + 
    geom_line(aes(y = upper), linetype = "dashed") + facet_wrap(~Variable, scales = "free_x", 
    labeller = as_labeller(var.names)) + geom_point(data = res.am, aes(x, y)) + ggtitle("Romanian Peasant Revolt Data (AM Fit)") + 
    geom_rug(data = res.am, aes(x, y), sides = "b", color = "blue") + theme(text = element_text(size = 15)) + 
    xlab("") + ylab("Smooth")

out.plpam <- gam(intensity ~ tradition + s(midpeasant, bs = "cr") + s(inequality, 
    bs = "cr") + commerce, data = Chirot, fam = gaussian)
summary(out.plpam)

plot.plpam <- visreg(out.plpam, type = "contrast", plot = FALSE)
smooths.plpam <- ldply(plot.plpam, function(part) data.frame(Variable = part$meta$x, 
    x = part$fit[[part$meta$x]], smooth = part$fit$visregFit, lower = part$fit$visregLwr, 
    upper = part$fit$visregUpr))

res.plpam <- ldply(plot.plpam, function(part) data.frame(Variable = part$meta$x, 
    x = part$res[[part$meta$x]], y = part$res$visregRes))

ggplot(smooths.plpam, aes(x, smooth)) + geom_line() + geom_line(aes(y = lower), linetype = "dashed") + 
    geom_line(aes(y = upper), linetype = "dashed") + facet_wrap(~Variable, scales = "free_x", 
    labeller = as_labeller(var.names)) + geom_point(data = res.plpam, aes(x, y)) + 
    ggtitle("Romanian Peasant Revolt Data (PLPAM Fit)") + geom_rug(data = res.plpam, 
    aes(x, y), sides = "b", color = "blue") + theme(text = element_text(size = 15)) + 
    xlab("") + ylab("Smooth")

out.plm <- kgplm(x = Chirot[, 2], t = Chirot[, 3:5], y = Chirot[, 1], h = c(10, 25, 
    1.5), family = "gaussian", link = "identity", kernel = "epanechnikov")
c(out.plm$b, sqrt(out.plm$bv))

grid.tradition <- seq(min(Chirot[, 3]), max(Chirot[, 3]), length = 20)
grid.midpeasant <- seq(min(Chirot[, 4]), max(Chirot[, 4]), length = 20)
grid.inequality <- seq(min(Chirot[, 5]), max(Chirot[, 5]), length = 20)
grid.commerce <- seq(min(Chirot[, 2]), max(Chirot[, 2]), length = 20)

grid1 <- cbind(grid.tradition, rep(mean(Chirot[, 4]), 20), rep(mean(Chirot[, 5]), 
    20))
out.plm1 <- kgplm(x = Chirot[, 2], t = Chirot[, 3:5], y = Chirot[, 1], grid = grid1, 
    h = c(10, 25, 1.5), family = "gaussian", link = "identity", kernel = "epanechnikov")

grid2 <- cbind(rep(mean(Chirot[, 3]), 20), grid.midpeasant, rep(mean(Chirot[, 5]), 
    20))
out.plm2 <- kgplm(x = Chirot[, 2], t = Chirot[, 3:5], y = Chirot[, 1], grid = grid2, 
    h = c(10, 25, 1.5), family = "gaussian", link = "identity", kernel = "epanechnikov")

grid3 <- cbind(rep(mean(Chirot[, 3]), 20), rep(mean(Chirot[, 4]), 20), grid.inequality)
out.plm3 <- kgplm(x = Chirot[, 2], t = Chirot[, 3:5], y = Chirot[, 1], grid = grid3, 
    h = c(10, 25, 1.5), family = "gaussian", link = "identity", kernel = "epanechnikov")

grid4 <- cbind(mean(Chirot[, 3]), mean(Chirot[, 4]), mean(Chirot[, 5]))
out.plm4 <- kgplm(x = Chirot[, 2], t = Chirot[, 3:5], y = Chirot[, 1], grid = grid4, 
    h = c(10, 25, 1.5), family = "gaussian", link = "identity", kernel = "epanechnikov")

y.hat <- Chirot[, 2] * out.plm$b + out.plm$m
plm.res <- Chirot[, 1] - pnorm(y.hat)

df1 <- data.frame(grid.tradition, out.plm1$m.grid, var.names = "Tradition")
df11 <- data.frame(Chirot$tradition, out.plm$m + plm.res)
p1 <- ggplot(data = df1, aes(x = grid.tradition, y = out.plm1$m.grid)) + geom_line() + 
    ylim(range(out.plm$m + plm.res)) + geom_point(data = df11, aes(x = Chirot$tradition, 
    y = out.plm$m + plm.res)) + ylab("Smooth") + geom_rug(data = df11, aes(x = Chirot$tradition, 
    y = Chirot$tradition), sides = "b", col = "blue") + facet_wrap(~var.names, scales = "free_x") + 
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(), text = element_text(size = 15))

df2 <- data.frame(grid.midpeasant, out.plm2$m.grid, var.names = "Midpeasant")
df22 <- data.frame(Chirot$midpeasant, out.plm$m + plm.res)
p2 <- ggplot(data = df2, aes(x = grid.midpeasant, y = out.plm2$m.grid)) + geom_line() + 
    ylim(range(out.plm$m + plm.res)) + geom_point(data = df22, aes(x = Chirot$midpeasant, 
    y = out.plm$m + plm.res)) + ylab("Smooth") + geom_rug(data = df22, aes(x = Chirot$midpeasant, 
    y = Chirot$midpeasant), sides = "b", col = "blue") + facet_wrap(~var.names, scales = "free_x") + 
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(), text = element_text(size = 15))

df3 <- data.frame(grid.inequality, out.plm3$m.grid, var.names = "Inequality")
df33 <- data.frame(Chirot$inequality, out.plm$m + plm.res)
p3 <- ggplot(data = df3, aes(x = grid.inequality, y = out.plm3$m.grid)) + geom_line() + 
    ylim(range(out.plm$m + plm.res)) + geom_point(data = df33, aes(x = Chirot$inequality, 
    y = out.plm$m + plm.res)) + ylab("Smooth") + geom_rug(data = df33, aes(x = Chirot$inequality, 
    y = Chirot$midpeasant), sides = "b", col = "blue") + facet_wrap(~var.names, scales = "free_x") + 
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(), text = element_text(size = 15))

df4 <- data.frame(grid.commerce, out.plm4$b * grid.commerce, var.names = "Commerce")
df44 <- data.frame(Chirot$commerce, out.plm$b * Chirot$commerce + plm.res)
p4 <- ggplot(data = df4, aes(x = grid.commerce, y = out.plm4$b * grid.commerce)) + 
    geom_line() + ylim(range(out.plm$b * Chirot$commerce + plm.res)) + geom_point(data = df44, 
    aes(x = Chirot$commerce, y = out.plm$b * Chirot$commerce + plm.res)) + ylab("Smooth") + 
    geom_rug(data = df44, aes(x = Chirot$commerce, y = Chirot$commerce), sides = "b", 
        col = "blue") + facet_wrap(~var.names, scales = "free_x") + theme(axis.title.x = element_blank(), 
    axis.title.y = element_blank(), text = element_text(size = 15))

title.grob <- textGrob(label = "Romanian Peasant Revolt Data (PLM Fit)", x = unit(0, 
    "lines"), y = unit(0, "lines"), hjust = -0.06, vjust = -0.1, gp = gpar(fontsize = 20))

grid.arrange(p1, p2, p3, p4, top = title.grob, left = textGrob("Smooth", rot = 90))

bw <- npindexbw(formula = intensity ~ Int + tradition + commerce, data = data.frame(Int = 1, 
    Chirot))
summary(bw)
model <- npindex(bws = bw, gradients = TRUE)
summary(model)
sim.data <- data.frame(Index = model$index, Intensity = predict(model), Obs.Intensity = Chirot$intensity)
sim.data <- sim.data[order(sim.data$Index), ]

ggplot(data = sim.data, aes(x = sim.data[, 1], y = sim.data[, 2])) + geom_line() + 
    xlab("Index") + ylab("Intensity") + ggtitle("Romanian Peasant Revolt Data (SIM Fit)") + 
    ylim(range(sim.data[, 3])) + geom_point(data = sim.data, aes(x = sim.data[, 1], 
    y = sim.data[, 3])) + theme(text = element_text(size = 15))

out.varcoef <- npscoef(intensity ~ commerce + tradition + midpeasant + inequality, 
    betas = TRUE, data = Chirot)
summary(out.varcoef)
colMeans(coef(out.varcoef))
out.comp <- round(rbind(colMeans(coef(out.varcoef)), coef(out.lm)), 4)
rownames(out.comp) <- c("lm", "vc")
out.comp

