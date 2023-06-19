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
### Example 23.7.2: Ragweed Data
###############################################################################

data(ragweed, package = "SemiPar")
ragweed$rain <- as.factor(ragweed$rain)
ragweed$rain <- mapvalues(ragweed$rain, c("0", "1"), c("Intense", "Otherwise"))

ggscatmat(ragweed[, -c(2, 5)]) + theme(text = element_text(size = 15),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

out.glm <- glm(ragweed ~ . - year, data = ragweed, family = poisson)
summary(out.glm)
var.names <- c(day.in.seas = "Days in Season", temperature = "Temperature", rain = "Rain Indicator", 
    wind.speed = "Wind Speed")

out.gam <- gam(ragweed ~ s(day.in.seas) + s(temperature), data = ragweed, family = poisson, 
    trace = TRUE)
summary(out.gam)
plot.gam <- visreg(out.gam, type = "contrast", plot = FALSE)
smooths <- ldply(plot.gam, function(part) data.frame(Variable = part$meta$x, x = part$fit[[part$meta$x]], 
    smooth = part$fit$visregFit, lower = part$fit$visregLwr, upper = part$fit$visregUpr))

res.gam <- ldply(plot.gam, function(part) data.frame(Variable = part$meta$x, x = part$res[[part$meta$x]], 
    y = part$res$visregRes))

ggplot(smooths, aes(x, smooth)) + geom_line() + geom_line(aes(y = lower), linetype = "dashed") + 
    geom_line(aes(y = upper), linetype = "dashed") + facet_wrap(~Variable, scales = "free_x", 
    labeller = as_labeller(var.names)) + geom_point(data = res.gam, aes(x, y)) + 
    ggtitle("Ragweed Data (GAM Fit)") + geom_rug(data = res.gam, aes(x, y), sides = "b", 
    color = "blue") + theme(text = element_text(size = 20)) + xlab("") + ylab("Smooth")

out.gplpam <- gam(ragweed ~ s(day.in.seas) + s(temperature) + rain + wind.speed, 
    data = ragweed, family = poisson, trace = TRUE)
summary(out.gplpam)
plot.gplpam <- visreg(out.gplpam, type = "contrast", plot = FALSE)
smooths <- ldply(plot.gplpam, function(part) data.frame(Variable = part$meta$x, x = part$fit[[part$meta$x]], 
    smooth = part$fit$visregFit, lower = part$fit$visregLwr, upper = part$fit$visregUpr))

res.gplpam <- ldply(plot.gplpam, function(part) data.frame(Variable = part$meta$x, 
    x = part$res[[part$meta$x]], y = part$res$visregRes))

p1 <- ggplot(subset(smooths, Variable == "day.in.seas"), aes(x, smooth)) + geom_line() + 
    geom_line(aes(y = lower), linetype = "dashed") + geom_line(aes(y = upper), linetype = "dashed") + 
    facet_wrap(~Variable, scales = "free_x", labeller = as_labeller(var.names)) + 
    geom_point(data = subset(res.gplpam, Variable == "day.in.seas"), aes(x, y)) + 
    geom_rug(data = subset(res.gplpam, Variable == "day.in.seas"), aes(x, y), sides = "b", 
        color = "blue") + ylim(-14, 17) + theme(text = element_text(size = 20), plot.margin = unit(c(0, 
    0, 0, 0), "cm")) + xlab("") + ylab("")

p2 <- ggplot(subset(smooths, Variable == "temperature"), aes(x, smooth)) + geom_line() + 
    geom_line(aes(y = lower), linetype = "dashed") + geom_line(aes(y = upper), linetype = "dashed") + 
    facet_wrap(~Variable, scales = "free_x", labeller = as_labeller(var.names)) + 
    geom_point(data = subset(res.gplpam, Variable == "temperature"), aes(x, y)) + 
    geom_rug(data = subset(res.gplpam, Variable == "temperature"), aes(x, y), sides = "b", 
        color = "blue") + ylim(-14, 17) + theme(text = element_text(size = 20), axis.text.y = element_blank(), 
    axis.ticks.y = element_blank(), plot.margin = unit(c(0, 0.5, 0, 0), "cm")) + 
    xlab("") + ylab("")

smooths2 <- subset(smooths, Variable == "rain")
res.gplpam2 <- subset(res.gplpam, Variable == "rain")
smooths2$x <- mapvalues(smooths2$x, c(1, 2), c("Intense", "Otherwise"))
res.gplpam2$x <- mapvalues(res.gplpam2$x, c(1, 2), c("Intense", "Otherwise"))

p3 <- ggplot(smooths2, aes(x, smooth, group = 1)) + geom_line() + geom_line(aes(y = lower), 
    linetype = "dashed") + geom_line(aes(y = upper), linetype = "dashed") + facet_wrap(~Variable, 
    scales = "free_x", labeller = as_labeller(var.names)) + geom_point(data = res.gplpam2, 
    aes(x, y, group = 1)) + geom_rug(data = res.gplpam2, aes(x, y, group = 1), sides = "b", 
    color = "blue") + ylim(-14, 17) + theme(text = element_text(size = 20), plot.margin = unit(c(0, 
    0, 0, 0), "cm")) + xlab("") + ylab("")

p4 <- ggplot(subset(smooths, Variable == "wind.speed"), aes(x, smooth)) + geom_line() + 
    geom_line(aes(y = lower), linetype = "dashed") + geom_line(aes(y = upper), linetype = "dashed") + 
    facet_wrap(~Variable, scales = "free_x", labeller = as_labeller(var.names)) + 
    geom_point(data = subset(res.gplpam, Variable == "wind.speed"), aes(x, y)) + 
    geom_rug(data = subset(res.gplpam, Variable == "wind.speed"), aes(x, y), sides = "b", 
        color = "blue") + ylim(-14, 17) + theme(text = element_text(size = 20), axis.text.y = element_blank(), 
    axis.ticks.y = element_blank(), plot.margin = unit(c(0, 0.5, 0, 0), "cm")) + 
    xlab("") + ylab("")

title.grob2 <- textGrob(label = "Ragweed Data (GPLPAM Fit)", x = unit(0, "lines"), 
    y = unit(0, "lines"), hjust = -0.18, vjust = -0.6, gp = gpar(fontsize = 20))

grid.arrange(p1, p2, p3, p4, top = title.grob2, left = textGrob("Smooth", rot = 90, 
    vjust = 1))

ragweed2 <- ragweed
ragweed2$rain <- as.logical(mapvalues(ragweed2$rain, c("Intense", "Otherwise"), c(TRUE, 
    FALSE)))

out.gplm <- kgplm(x = ragweed2[, c(1, 6)], t = ragweed2[, 3:4], y = ragweed2[, 5], 
    h = c(20, 5), family = "bernoulli", link = "probit", kernel = "epanechnikov")
coef.out <- round(cbind(out.gplm$b, sqrt(diag(out.gplm$bv))), 4)
colnames(coef.out) <- c("Coef", "SE")
coef.out

grid.ragweed <- seq(min(ragweed2[, 1]), max(ragweed2[, 1]), length = 20)
grid.day.in.seas <- seq(min(ragweed2[, 3]), max(ragweed2[, 3]), length = 20)
grid.temperature <- seq(min(ragweed2[, 4]), max(ragweed2[, 4]), length = 20)
grid.wind.speed <- seq(min(ragweed2[, 6]), max(ragweed2[, 6]), length = 20)

grid1 <- cbind(grid.day.in.seas, rep(mean(ragweed2[, 4]), 20))
out.gplm1 <- kgplm(x = ragweed2[, c(1, 6)], t = ragweed2[, 3:4], y = ragweed2[, 5], 
    grid = grid1, h = c(20, 5), family = "bernoulli", link = "probit", kernel = "epanechnikov")

grid2 <- cbind(rep(mean(ragweed2[, 3]), 20), grid.temperature)
out.gplm2 <- kgplm(x = ragweed2[, c(1, 6)], t = ragweed2[, 3:4], y = ragweed2[, 5], 
    grid = grid2, h = c(20, 5), family = "bernoulli", link = "probit", kernel = "epanechnikov")

grid3 <- cbind(rep(mean(ragweed2[, 3]), 2), rep(mean(ragweed2[, 4]), 2))
out.gplm3 <- kgplm(x = ragweed2[, c(1, 6)], t = ragweed2[, 3:4], y = ragweed2[, 5], 
    grid = grid3, h = c(20, 5), family = "bernoulli", link = "probit", kernel = "epanechnikov")

y.hat <- c(as.matrix(ragweed2[, c(1, 6)]) %*% out.gplm$b) + out.gplm$m
gplm.res <- ragweed2[, 5] - pnorm(y.hat)

ddf1 <- data.frame(x = grid.ragweed, y = pnorm(out.gplm3$m.grid[1] + c(cbind(grid.ragweed, 
    mean(ragweed2[, 6])) %*% out.gplm3$b)), var.names = "Ragweed Count")
ddf11 <- data.frame(x = ragweed2$ragweed, y = as.numeric(ragweed2$rain))
pp1 <- ggplot(data = ddf1, aes(x = x, y = y)) + geom_line() + ylim(c(-0.1, 1.1)) + 
    geom_point(data = ddf11, aes(x = x, y = y)) + facet_wrap(~var.names, scales = "free_x") + 
    geom_rug(data = ddf11, aes(x = x, y = x), sides = "b", col = "blue") + theme(text = element_text(size = 20), 
    plot.margin = unit(c(0, 0, 0, 0), "cm")) + xlab("") + ylab("")

ddf2 <- data.frame(x = grid.day.in.seas, y = pnorm(out.gplm1$m.grid + c(cbind(rep(mean(ragweed2[, 
    1]), 20), rep(mean(ragweed2[, 6]), 20)) %*% out.gplm1$b)), var.names = "Days in Season")
ddf22 <- data.frame(x = ragweed2$day.in.seas, y = as.numeric(ragweed2$rain))
pp2 <- ggplot(data = ddf2, aes(x = x, y = y)) + geom_line() + ylim(c(-0.1, 1.1)) + 
    geom_point(data = ddf22, aes(x = x, y = y)) + facet_wrap(~var.names, scales = "free_x") + 
    geom_rug(data = ddf22, aes(x = x, y = x), sides = "b", col = "blue") + theme(text = element_text(size = 20), 
    axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = unit(c(0, 
        0.5, 0, 0), "cm")) + xlab("") + ylab("")

ddf3 <- data.frame(x = grid.temperature, y = pnorm(out.gplm2$m.grid + c(cbind(rep(mean(ragweed2[, 
    1]), 20), rep(mean(ragweed2[, 6]), 20)) %*% out.gplm2$b)), var.names = "Temperature")
ddf33 <- data.frame(x = ragweed2$temperature, y = as.numeric(ragweed2$rain))
pp3 <- ggplot(data = ddf3, aes(x = x, y = y)) + geom_line() + ylim(c(-0.1, 1.1)) + 
    geom_point(data = ddf33, aes(x = x, y = y)) + facet_wrap(~var.names, scales = "free_x") + 
    geom_rug(data = ddf33, aes(x = x, y = x), sides = "b", col = "blue") + theme(text = element_text(size = 20), 
    plot.margin = unit(c(0, 0, 0, 0), "cm")) + xlab("") + ylab("")

ddf4 <- data.frame(x = grid.wind.speed, y = pnorm(out.gplm3$m.grid[1] + c(cbind(mean(ragweed2[, 
    1]), grid.wind.speed) %*% out.gplm3$b)), var.names = "Wind Speed")
ddf44 <- data.frame(x = ragweed2$wind.speed, y = as.numeric(ragweed2$rain))
pp4 <- ggplot(data = ddf4, aes(x = x, y = y)) + geom_line() + ylim(c(-0.1, 1.1)) + 
    geom_point(data = ddf44, aes(x = x, y = y)) + facet_wrap(~var.names, scales = "free_x") + 
    geom_rug(data = ddf44, aes(x = x, y = x), sides = "b", col = "blue") + theme(text = element_text(size = 20), 
    axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = unit(c(0, 
        0.5, 0, 0), "cm")) + xlab("") + ylab("")

title.grob3 <- textGrob(label = "Ragweed Data (GPLM Fit)", x = unit(0, "lines"), 
    y = unit(0, "lines"), hjust = -0.19, vjust = -0.4, gp = gpar(fontsize = 20))

grid.arrange(pp1, pp2, pp3, pp4, top = title.grob3, left = textGrob("Smooth", rot = 90, 
    vjust = 1))

