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
### Example 25.11.1: Annual Maximum Sea Levels Data
###############################################################################

data(fremantle, package = "ismev")
out <- fevd(fremantle$SeaLevel, fremantle, location.fun = ~I(Year - 1896) + SOI, 
    use.phi = TRUE)

out1 <- fevd(fremantle$SeaLevel, fremantle, location.fun = ~I(Year - 1896) + SOI, 
    use.phi = TRUE)
out2 <- fevd(fremantle$SeaLevel, fremantle, location.fun = ~I(Year - 1896) + SOI, 
    scale.fun = ~I(Year - 1896) + SOI, use.phi = TRUE)
out3 <- fevd(fremantle$SeaLevel, fremantle, location.fun = ~I(Year - 1896) + SOI, 
    scale.fun = ~I(Year - 1896) + SOI, shape.fun = ~I(Year - 1896) + SOI, use.phi = TRUE)
out1
out2
out3
X1 <- cbind(1, fremantle$Year - 1896, -1)
X2 <- cbind(1, fremantle$Year - 1896, 0)
X3 <- cbind(1, fremantle$Year - 1896, 1)
X4 <- cbind(1, fremantle$Year - 1896, 2)
mean.fun1 <- X1 %*% cbind(out1$results$par[1:3])
mean.fun2 <- X2 %*% cbind(out1$results$par[1:3])
mean.fun3 <- X3 %*% cbind(out1$results$par[1:3])
mean.fun4 <- X4 %*% cbind(out1$results$par[1:3])

df13 <- data.frame(x = fremantle$Year, y = fremantle$SeaLevel)
df14 <- data.frame(x = rep(fremantle$Year, 4), y = c(mean.fun1, mean.fun2, mean.fun3, 
    mean.fun4), SOI = c(rep("-1", 86), rep("0", 86), rep("1", 86), rep("2", 86)))
ggplot(data = df13, aes(x, y)) + geom_point() + geom_line(data = df14, aes(x, y, 
    col = SOI, linetype = SOI), lwd = 1.2) + xlab("Year") + ylab("Sea Level") + ggtitle("Annual Maximum Sea Levels Data") + 
    theme(text = element_text(size = 20))

df <- data.frame(x = fremantle$Year, y.fit = mean.fun1, y = fremantle$SeaLevel - 
    mean.fun1)
ggplot(df, aes(sample = y)) + stat_qq(size = 3) + theme(text = element_text(size = 20)) + 
    xlab("Theoretical") + ylab("Residuals") + ggtitle("Q-Q Plot of the Residuals")

