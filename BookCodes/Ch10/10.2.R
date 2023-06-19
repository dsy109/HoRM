library(ggplot2)
library(sandwich)
library(fastR)
library(car)
library(MASS)
library(strucchange)
library(outliers)
library(influence.ME)
library(climtrends)
library(faraway)
library(het.test)

###############################################################################
### Example 10.5.2: Expenditures Data
###############################################################################

data(PublicSchools, package = "sandwich")
PublicSchools[is.na(PublicSchools)] <- 0
PublicSchools <- PublicSchools[-50, ]
out.pub <- lm(Expenditure ~ Income, data = PublicSchools)
summary(out.pub)

ggplot(PublicSchools, aes(x = Income, y = Expenditure)) + geom_point(size = 3) + 
    theme(text = element_text(size = 20)) + xlab("Per Capita Income (U.S. Dollars)") + 
    ylab("Per Capita Expenditures (U.S. Dollars)") + geom_smooth(method = lm) + ggtitle("Expenditures Data")

ggplot(PublicSchools, aes(fitted.values(out.pub), studres(out.pub))) + geom_point(size = 3) + 
    theme(text = element_text(size = 20)) + xlab("Fitted Values") + ylab("Studentized Residuals") + 
    geom_hline(yintercept = 0, color = 1, size = 0.6) + ggtitle("Studentized Residuals")

data.new <- data.frame(fit = fitted.values(out.pub)[-c(1, 2)], rr = recresid(out.pub))
ggplot(data.new, aes(fitted.values(out.pub)[-c(1, 2)], recresid(out.pub))) + geom_point(size = 3) + 
    theme(text = element_text(size = 20)) + xlab("Fitted Values") + ylab("Recursive Residuals") + 
    geom_hline(yintercept = 0, color = 1, size = 0.6) + ggtitle("Recursive Residuals")

grubbs.test(resid(out.pub), opposite = F, type = 10)  #maximum
grubbs.test(resid(out.pub), opposite = T, type = 10)  #minimum

set.seed(100)
TM.out <- t(sapply(1:9, function(i) unlist(FindOutliersTietjenMooreTest(resid(out.pub), 
    k = i))))
rownames(TM.out) <- 1:9
Sig <- ifelse(TM.out[, 1] < TM.out[, 2], "Significant", "Not Significant")
TM.out <- data.frame(TM.out, Sig = Sig)
TM.out

chisq.out.test(resid(out.pub), opposite = F)  #maximum
chisq.out.test(resid(out.pub), opposite = T)  #minimum

FindOutliersESDtest(resid(out.pub), k = 9)

hatvalues <- hatvalues(out.pub)
stand <- rstandard(out.pub)
hadis <- hatvalues/(1 - hatvalues) + (2 + 1)/(1 - hatvalues) * (stand/(1 - stand))
ggplot(PublicSchools, aes(fitted.values(out.pub), hadis)) + geom_point(size = 3) + 
    theme(text = element_text(size = 20)) + xlab("Fitted Values") + ylab("Hadi's") + 
    geom_hline(yintercept = 0, color = 1, size = 0.6) + ggtitle("Hadi's Influence Measure")

dfbetas(out.pub)
newpub <- cbind(PublicSchools, dfbetas(out.pub), fitted.values(out.pub))
names(newpub) <- c("Expenditure", "Income", "dfbeta_zero", "dfbeta_1", "fitted")
ggplot(newpub, aes(x = fitted, y = newpub$dfbeta_zero)) + geom_point(size = 3) + 
    ggtitle("Expenditures Data") + theme(text = element_text(size = 20)) + xlab("Fitted values") + 
    ylab(expression(DFBETAS[(1)])) + geom_hline(yintercept = 0) + ggtitle(expression(DFBETAS[(1)]))

ggplot(newpub, aes(x = fitted, y = newpub$dfbeta_1)) + geom_point(size = 3) + ggtitle("Expenditures Data") + 
    theme(text = element_text(size = 20)) + xlab("Fitted values") + ylab(expression(DFBETAS[(2)])) + 
    geom_hline(yintercept = 0) + ggtitle(expression(DFBETAS[(2)]))

# Glejser's Test
ares <- abs(residuals(out.pub))
g1 <- summary(lm(ares ~ Income, data = PublicSchools))
g2 <- summary(lm(ares ~ sqrt(Income), data = PublicSchools))
g3 <- summary(lm(ares ~ I(1/Income), data = PublicSchools))
c(g1$r.squared, g2$r.squared, g3$r.squared)  #g1 has highest R2
g1

# White's Test
model <- VAR(PublicSchools)
whites.htest(model)

# Goldfeld-Quandt Test
gqtest(out.pub)

# Ramsey's RESET
resettest(out.pub)

# Rainbow Test
raintest(out.pub)

# Harvey-Collier Test
harvtest(out.pub)

