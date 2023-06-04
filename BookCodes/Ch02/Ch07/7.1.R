library(ggplot2)
library(HoRM)
library(GGally)
library(MASS)
library(MPV)
library(car)
library(perturb)

###############################################################################
### Example 7.4.1: Thermal Energy Data, cont'd
###############################################################################

data(solar, package = "MPV")
names(solar) <- c("heat.flux", "insolation", "east", "south", "north", "time")
solar2 <- data.frame(solar, west = solar$east/2)

X <- as.matrix(solar2[, -1])
solve(t(X) %*% X) #This will give an error.

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

ggscatmat(solar2) + theme(text = element_text(size = 11), 
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

