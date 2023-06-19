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
### Example 25.8.1: Animal Weight Data (cont.)
###############################################################################

data(Animals, package = "MASS")
Animals0 <- list(y = log10(Animals$brain), X = cbind(1, log10(Animals$body)))

beta.rd <- c(0.981, 0.703)
Var.0 <- 50 * diag(2)
Var.1 <- 0.01 * diag(2)
nu.0 <- 3
ssq.0 <- var(log10(Animals$brain))
Prior1 <- list(betabar = beta.rd, A = Var.0, nu = nu.0, ssq = ssq.0)
Prior2 <- list(betabar = beta.rd, A = Var.1, nu = nu.0, ssq = ssq.0)

R <- 10000
MCMC <- list(R = R, keep = 1)

set.seed(100)
simOut1 <- runiregGibbs(Animals0, Prior1, MCMC)
simOutcond1 <- data.frame(simOut1[[1]][, 1], simOut1[[1]][, 2], simOut1[[2]])
names(simOutcond1) <- c("beta[0]", "beta[1]", "sigma^2")
simOutcond1 <- as.mcmc(simOutcond1)
set.seed(100)
simOut2 <- runiregGibbs(Animals0, Prior2, MCMC)
simOutcond2 <- data.frame(simOut2[[1]][, 1], simOut2[[1]][, 2], simOut2[[2]])
names(simOutcond2) <- c("beta[0]", "beta[1]", "sigma^2")
simOutcond2 <- as.mcmc(simOutcond2)
simOutcond <- mcmc.list(simOutcond1, simOutcond2)
S <- ggs(simOutcond)

ggs_density(S, greek = TRUE) + ggtitle("Posterior Distributions") + theme(text = element_text(size = 20)) + 
    xlab("Value") + ylab("Density")

ggs_traceplot(S, greek = TRUE) + ggtitle("Trace Plots") + theme(text = element_text(size = 20)) + 
    ylab("Value")

ggs_running(S, greek = TRUE) + ggtitle("Running Means") + theme(text = element_text(size = 20))

ggs_autocorrelation(S, greek = TRUE) + ggtitle("Autocorrelation Plots") + theme(text = element_text(size = 20))

post.means1 <- apply(simOutcond1, 2, mean)
post.means1
apply(simOutcond1, 2, quantile, c(0.025, 0.975))
post.means2 <- apply(simOutcond2, 2, mean)
post.means2
apply(simOutcond2, 2, quantile, c(0.025, 0.975))

Animals.log <- data.frame(Brain = log10(Animals$brain), Body = log10(Animals$body))

smDf <- data.frame(intercept = c(post.means1[1], post.means2[1]), slope = c(post.means1[2], 
    post.means2[2]), color = c("#56B4E9", "#FF9999"), linetype = factor(c(1, 2)))

ggplot(Animals.log, aes(x = Body, y = Brain)) + geom_point(size = 3) + geom_abline(aes(intercept = intercept, 
    slope = slope, linetype = linetype, color = color), lwd = 1.3, data = smDf, show.legend = TRUE) + 
    scale_colour_manual(name = "Chain", values = 1:2, labels = c("Chain 1", "Chain 2")) + 
    scale_linetype_manual(name = "Chain", values = 1:2, labels = c("Chain 1", "Chain 2")) + 
    ggtitle("Animal Weight Data") + theme(text = element_text(size = 20)) + ylab("Brain Weight (kg)") + 
    xlab("Body Weight (kg)")

