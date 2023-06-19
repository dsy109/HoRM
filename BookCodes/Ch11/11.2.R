library(ggplot2)
library(HoRM)
library(MethComp)
library(deming)
library(sem)

###############################################################################
### Example 11.6.2: U.S. Economy Data
###############################################################################

data(Kmenta, package = "sem")

names(Kmenta) <- c("Y", "X1", "X2", "X3", "X4")
summary(tsls(Y ~ X1 + X3 + X4, ~X2 + X3 + X4, data = Kmenta))  # supply equation
summary(tsls(Y ~ X1 + X2, ~X2 + X3 + X4, data = Kmenta))  # demand equation

attach(Kmenta)
out1 <- lm(cbind(1, X1, X3, X4) ~ cbind(X2, X3, X4))
X.hat <- out1$fitted
lm(Y ~ X.hat - 1)

