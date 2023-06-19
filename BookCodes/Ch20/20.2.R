library(ggplot2)
library(HoRM)
library(pROC)
library(lmtest)
library(mlogit)
library(nnet)
library(foreign)
library(pscl)
library(msme)
library(gamlss.tr)
library(VGAM)
library(reshape)

###############################################################################
### Example 20.5.2: Sportsfishing Survey Data
###############################################################################

data(Fishing, package = "mlogit")
out.fish <- multinom(mode ~ . - catch.charter - catch.beach - catch.boat - catch.pier, 
    data = Fishing)
out.fish.int <- multinom(mode ~ 1, data = Fishing)
sum.out <- summary(out.fish)
p.vals <- (1 - pnorm(abs(summary(out.fish)$coefficients/summary(out.fish)$standard.errors))) * 
    2
list(Coef = round(sum.out$coefficients[, -1], 4), SE = round(sum.out$standard.errors[, 
    -1], 4), p.vals = round(p.vals[, -1], 4))

round(suppressWarnings(exp(cbind(OR = coef(out.fish))))[, -1], 4)

lmtest::lrtest(out.fish.int, out.fish)

new.data <- data.frame(sort(rep(levels(Fishing$mode), 116)), mean(Fishing[, 2]), 
    mean(Fishing[, 3]), mean(Fishing[, 4]), mean(Fishing[, 5]), mean(Fishing[, 6]), 
    mean(Fishing[, 7]), mean(Fishing[, 8]), mean(Fishing[, 9]), rep(seq(500, 12000, 
        by = 100), 4))
colnames(new.data) <- colnames(Fishing)
py <- cbind(melt(predict(out.fish, newdata = new.data, type = "probs", se = TRUE))[, 
    -1], rep(seq(500, 12000, by = 100), 4))
colnames(py) <- c("Mode", "Probability", "Income")
ggplot(py, aes(x = Income, y = Probability, color = Mode, lty = Mode)) + geom_line(lwd = 1.2) + 
    theme(text = element_text(size = 20)) + ggtitle("Predicted Probabilities")

