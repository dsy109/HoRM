library(ggplot2)
library(HoRM)
library(quantmod)
library(astsa)
library(orcutt)
library(car)
library(forecast)
library(nlme)
library(HarmonicRegression)
library(prais)
library(lmtest)

###############################################################################
### Example 13.5.5: Mouse Liver Data
###############################################################################

data(rna.nasc, package = "HarmonicRegression")
nasc.t <- seq(0, 44, 4)
x.t <- rna.nasc["Arntl", -1]
HR.out <- harmonic.regression(as.numeric(x.t), nasc.t, Tau = 24)
HR.out
HR.fit <- ts(HR.out$fit.vals, start = 0, end = 44, frequency = 1/4)
HR2 <- data.frame(Time = rep(seq(0, 44, by = 4), 2), TS = c(as.numeric(x.t), as.numeric(HR.fit)), 
    Series = as.factor(c(rep("Data", 12), rep("HarmReg", 12))))
ggplot(HR2, aes(x = Time, y = TS, group = Series)) + ylab("Transcriptional Activity") + 
    ggtitle("Mouse Liver Data") + geom_line(aes(col = Series, lty = Series), lwd = 1.1) + 
    theme(text = element_text(size = 20))



