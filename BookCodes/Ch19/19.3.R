library(ggplot2)
library(HoRM)
library(nlstools)

###############################################################################
### Example 19.4.3: James Bond Data
###############################################################################

data(JamesBond, package = "HoRM")

bond.mod <- JamesBond$US_Gross/1e+06
year <- JamesBond$Year
pop.mod <- nls(bond.mod ~ beta0/(1 + exp(beta1 + beta2 * (year - 1962))), start = list(beta0 = 1000, 
    beta1 = 4, beta2 = -0.05), trace = F)
summary(pop.mod)

bond.res <- nlsResiduals(pop.mod)
bond.df <- data.frame(year = year, bond.mod = bond.mod, fits = bond.res$resi2[, 1], 
    res = bond.res$resi2[, 2])

new.x <- seq(1962, 2015, len = 1000)
df2 <- data.frame(year = new.x)
df2$bond.mod <- predict(pop.mod, list(year = new.x))

ggplot(data = bond.df, aes(x = year, y = bond.mod)) + geom_point(size = 3) + geom_line(data = df2, 
    aes(x = year, y = bond.mod), col = 2) + ggtitle("James Bond Data") + theme(text = element_text(size = 20)) + 
    xlab("Year") + ylab("U.S. Gross in Millions of Dollars (Unadjusted)")

res <- data.frame(x = fitted.values(orl.nl2), y = resid(orl.nl2))
ggplot(bond.df, aes(x = fits, y = res)) + geom_point(size = 3) + theme(text = element_text(size = 20)) + 
    xlab("Fitted Values") + ylab("Standardized Residuals") + geom_hline(yintercept = 0, 
    color = 1, size = 0.6) + ggtitle("Standardized Residuals") + ylim(-3, 3) + geom_hline(yintercept = c(-3, 
    3), linetype = "dashed")

round(summary(pop.mod)$coefficients[, 1] - qt(1 - (0.05/6), 21) * summary(pop.mod)$coefficients[, 
    2], 3)
round(summary(pop.mod)$coefficients[, 1] + qt(1 - (0.05/6), 21) * summary(pop.mod)$coefficients[, 
    2], 3)
