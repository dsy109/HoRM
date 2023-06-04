library(ggplot2)
library(HoRM)
library(aprean3)
library(tolerance)
library(psych)
library(investr)

###############################################################################
### Example 3.6.3: Pulp Property Data
###############################################################################

data(wood, package = "HoRM")

r <- cor(wood)
z <- FisherZ(r)
tanh(z + c(-1, 1) * qnorm(0.975) * sqrt(1/4))

out <- lm(pulp ~ Kappa, data = wood)
summary(out)

cal.inv <- calibrate(out, y0 = c(51, 50, 52), interval = "inversion", level = 0.9)
cal.cent <- calibrate(out, y0 = c(51, 50, 52), interval = "Wald", level = 0.9)
cal.inv
cal.cent

reg.inv <- calibrate(out, y0 = 51, interval = "inversion", level = 0.9, mean.response = TRUE)
reg.cent <- calibrate(out, y0 = 51, interval = "Wald", level = 0.9, mean.response = TRUE)
reg.inv
reg.cent

ggplot(wood, aes(x = Kappa, y = pulp)) + geom_point(size = 3) + geom_smooth(method = lm, 
    se = FALSE) + ggtitle("Pulp Property Data") + theme(text = element_text(size = 20)) + 
    xlab("Kappa Number") + ylab("Pulp Yield (%)") + geom_vline(xintercept = cal.cent$estimate, 
    color = 1, size = 1.3) + geom_vline(xintercept = as.numeric(cal.cent[2:3]), color = 2, 
    linetype = 2, size = 1.3) + geom_vline(xintercept = as.numeric(reg.cent[2:3]), 
    color = 3, linetype = 3, size = 1.3)



