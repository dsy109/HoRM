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
### Example 25.4.1: Mediation Regression Example
###############################################################################

# Diagram in Main Discussion
plot(c(0, 100), c(-30, 90), type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")

rect(0, 0, 25, 25, lwd = 3, col = "gray88")
rect(37.5, 65, 62.5, 90, lwd = 3, col = "gray88")
rect(75, 0, 100, 25, lwd = 3, col = "gray88")

arrows(26, 30, 36.5, 60, lwd = 3)
arrows(63.5, 60, 74, 30, lwd = 3)
arrows(28, 12.5, 72, 12.5, lwd = 3)

text(c(12.5, 50, 87.5), c(12.5, 77.5, 12.5), labels = c("X", "M", "Y"), cex = 2)

text(c(31.25, 68.75, 50), c(45, 45, 12.5), labels = c("A", "B", "C"), pos = c(4, 
    2, 3), cex = 1.5)

rect(40, -30, 60, -10, lwd = 2, lty = 2)
rect(2.5, 57.5, 22.5, 77.5, lwd = 2, lty = 2)
rect(77.5, 57.5, 97.5, 77.5, lwd = 2, lty = 2)

arrows(50, -8, 50, 10.5, lwd = 2, lty = 2)
arrows(23.5, 56.5, 30.25, 46, lwd = 2, lty = 2)
arrows(76.5, 56.5, 69.75, 46, lwd = 2, lty = 2)

text(c(12.5, 50, 87.5), c(67.5, -20, 67.5), labels = c(expression(W[1]), expression(W[2]), 
    expression(W[3])), cex = 2)

# Example
data(bh1996, package = "multilevel")
bh1996.c <- data.frame(bh1996, HRS.c = scale(bh1996$HRS, scale = F), LEAD.c = scale(bh1996$LEAD, 
    scale = F), COHES.c = scale(bh1996$COHES, scale = F))
moderate.out <- lm(WBEING ~ HRS.c * COHES.c + LEAD.c, data = bh1996.c)
summary(moderate.out)

mediate.out <- sobel(pred = bh1996$HRS, med = bh1996$COHES, out = bh1996$WBEING)
mediate.out

