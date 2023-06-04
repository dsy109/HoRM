library(ggplot2)
library(HoRM)
library(MPV)
library(car)

###############################################################################
### Example 8.6.1: Thermal Energy Data, cont'd
###############################################################################

data(solar, package = "MPV")
names(solar) <- c("heat.flux", "insolation", "east", "south", "north", "time")

out <- lm(heat.flux ~ ., data = solar)
out.aov <- reg.anova(out)
out.aov

out2 <- lm(heat.flux ~ north + south, data = solar)
out2.aov <- reg.anova(out2)
out2.aov

anova(out2, out)

power.F(full = out, reduced = out2, alpha = 0.05)

out <- lm(heat.flux ~ east + north + south + time + insolation, data = solar)
summary(out)
vif(out)
W <- scale(solar[, 2:6])
tmp1 <- sqrt(eigen(t(W) %*% W)$values)
tmp1
colldiag(out, add.intercept = FALSE)

out2 <- lm(heat.flux ~ east + north + south + insolation, data = solar)
summary(out2)
vif(out2)
1/vif(out2)
Z <- scale(solar[, 2:5])
tmp <- sqrt(eigen(t(Z) %*% Z)$values)
tmp
colldiag(out2, add.intercept = FALSE)

