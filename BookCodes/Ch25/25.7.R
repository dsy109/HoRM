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
### Example 25.7.1: Mammal Sleep Data
###############################################################################

data(sleep, package = "VIM")
sleep.impute <- sleep
ind.na <- apply(is.na(sleep[, 4:7]), 2, which)
sapply(ind.na, length)

comp.case <- lm(Sleep ~ . - NonD, data = sleep)
summary(comp.case)

# Regression Imputation for Gestation
gest.impute <- lm(Gest ~ BodyWgt + BrainWgt, data = sleep)
gest.impute
gest.impute.pred <- round(predict(gest.impute, data = sleep[, 1:2]))
sleep.impute$Gest[ind.na$Gest] <- gest.impute.pred[ind.na$Gest]

# Deductive Imputation for Span
sleep.order <- sleep[order(sleep$BodyWgt, sleep$BrainWgt), ]
span.impute <- sleep.order[, 6]
span.na <- which(is.na(span.impute))
span.impute <- sapply(1:length(span.na), function(i) round(mean(span.impute[c(span.na[i] - 
    1, span.na[i] + 1)]), 1))
sleep.impute$Span[as.numeric(rownames(sleep.order)[span.na])] <- span.impute

# Mean Imputation for Sleep
sleep.impute$Sleep[ind.na$Sleep] <- round(mean(sleep.impute$Sleep, na.rm = T), 1)

# Deductive Imputation Based on Mean Proportion of Dream
mean.prop <- mean(sleep.impute$Dream/sleep.impute$Sleep, na.rm = T)
sleep.impute$Dream[ind.na$Dream] <- round(mean.prop * sleep.impute$Sleep[ind.na$Dream], 
    1)

Gest.dens <- density(sleep$Gest[-ind.na$Gest])
Span.dens <- density(sleep$Span[-ind.na$Span])
Sleep.dens <- density(sleep$Sleep[-ind.na$Sleep])
Dream.dens <- density(sleep$Dream[-ind.na$Dream])

Gest.dens.impute <- density(sleep.impute$Gest)
Span.dens.impute <- density(sleep.impute$Span)
Sleep.dens.impute <- density(sleep.impute$Sleep)
Dream.dens.impute <- density(sleep.impute$Dream)

Gest.2 <- data.frame(Gest = c(Gest.dens$x, Gest.dens.impute$x), Density = c(Gest.dens$y, 
    Gest.dens.impute$y), Data = as.factor(c(rep("Observed", length(Gest.dens$x)), 
    rep("Imputed", length(Gest.dens.impute$x)))))
Span.2 <- data.frame(Span = c(Span.dens$x, Span.dens.impute$x), Density = c(Span.dens$y, 
    Span.dens.impute$y), Data = as.factor(c(rep("Observed", length(Span.dens$x)), 
    rep("Imputed", length(Span.dens.impute$x)))))
Sleep.2 <- data.frame(Sleep = c(Sleep.dens$x, Sleep.dens.impute$x), Density = c(Sleep.dens$y, 
    Sleep.dens.impute$y), Data = as.factor(c(rep("Observed", length(Sleep.dens$x)), 
    rep("Imputed", length(Sleep.dens.impute$x)))))
Dream.2 <- data.frame(Dream = c(Dream.dens$x, Dream.dens.impute$x), Density = c(Dream.dens$y, 
    Dream.dens.impute$y), Data = as.factor(c(rep("Observed", length(Dream.dens$x)), 
    rep("Imputed", length(Dream.dens.impute$x)))))

ggplot(Gest.2, aes(x = Gest, y = Density, group = Data)) + geom_line(aes(color = Data, 
    lty = Data), size = 1.5) + ggtitle("Kernel Density Fits for Gestation Time") + 
    theme(text = element_text(size = 20)) + xlab("Gestation Time (Days)")
ggplot(Span.2, aes(x = Span, y = Density, group = Data)) + geom_line(aes(color = Data, 
    lty = Data), size = 1.5) + ggtitle("Kernel Density Fits for Life Span") + theme(text = element_text(size = 20)) + 
    xlab("Life Span (Years)")
ggplot(Sleep.2, aes(x = Sleep, y = Density, group = Data)) + geom_line(aes(color = Data, 
    lty = Data), size = 1.5) + ggtitle("Kernel Density Fits for Total Sleep Time") + 
    theme(text = element_text(size = 20)) + xlab("Total Sleep Time (Hours)")
ggplot(Dream.2, aes(x = Dream, y = Density, group = Data)) + geom_line(aes(color = Data, 
    lty = Data), size = 1.5) + ggtitle("Kernel Density Fits for Paradoxical Sleep Time") + 
    theme(text = element_text(size = 20)) + xlab("Paradoxical Sleep Time (Hours)")

impute.case <- lm(Sleep ~ ., data = sleep.impute[, -3])
summary(impute.case)

