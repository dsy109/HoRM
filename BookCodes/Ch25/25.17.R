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
### Example 25.17.1: NASA Data
###############################################################################

data(nasa, package = "dplyr")

nasa.df <- as.data.frame(dplyr::nasa)
cloudlow.ind <- which(is.na(nasa.df$cloudlow))
for (i in 1:length(cloudlow.ind))
{
    nasa.df$cloudlow[cloudlow.ind] <- round(mean(nasa.df$cloudlow[cloudlow.ind + 
        c(-5:5)], na.rm = T))
}

dim.t <- c(lat = 24, long = 24, month = 12, year = 6)

cloudhigh.t <- rTensor::as.tensor(array(nasa.df$cloudhigh, dim = dim.t))
cloudlow.t <- rTensor::as.tensor(array(nasa.df$cloudlow, dim = dim.t))
cloudmid.t <- rTensor::as.tensor(array(nasa.df$cloudmid, dim = dim.t))
ozone.t <- rTensor::as.tensor(array(nasa.df$ozone, dim = dim.t))
pressure.t <- rTensor::as.tensor(array(nasa.df$pressure, dim = dim.t))
surftemp.t <- rTensor::as.tensor(array(nasa.df$surftemp, dim = dim.t))
temperature.t <- rTensor::as.tensor(array(nasa.df$temperature, dim = dim.t))

cloudhigh.tucker <- tucker(cloudhigh.t, ranks = c(3, 3, 3, 3))
cloudlow.tucker <- tucker(cloudlow.t, ranks = c(3, 3, 3, 3))
cloudmid.tucker <- tucker(cloudmid.t, ranks = c(3, 3, 3, 3))
ozone.tucker <- tucker(ozone.t, ranks = c(3, 3, 3, 3))
pressure.tucker <- tucker(pressure.t, ranks = c(3, 3, 3, 3))
surftemp.tucker <- tucker(surftemp.t, ranks = c(3, 3, 3, 3))
temperature.tucker <- tucker(temperature.t, ranks = c(3, 3, 3, 3))

out.beta.tucker <- vector("list", 3)
for (i in 1:3) out.beta.tucker[[i]] <- vector("list", 3)

for (i in 1:3)
{
    for (j in 1:3)
    {
        out.beta.tucker[[i]][[j]] <- lm(vec(ozone.tucker$Z[, , j, i]) ~ vec(cloudhigh.tucker$Z[, 
            , j, i]) + vec(cloudmid.tucker$Z[, , j, i]) + vec(cloudlow.tucker$Z[, 
            , j, i]) + vec(pressure.tucker$Z[, , j, i]) + vec(surftemp.tucker$Z[, 
            , j, i]) + vec(temperature.tucker$Z[, , j, i]))$coef
    }
}

beta.tensor <- to.tensor(c(unlist(out.beta.tucker[[1]]), unlist(out.beta.tucker[[2]]), 
    unlist(out.beta.tucker[[3]])), dim = c(7, 3, 3))
rownames(beta.tensor) <- c("(Intercept)", "cloudhigh", "cloudmid", "cloudlow", "pressure", 
    "surftemp", "temperature")
round(beta.tensor, 3)

