library(ggplot2)
library(HoRM)
library(MASS)
library(aprean3)
library(alr3)

###############################################################################
### Example 5.3.3: Fiber Strength Data
###############################################################################

data(fiber, package = "HoRM")

ggplot(fiber, aes(x = pressure, y = tensile)) + geom_point(size = 3) + geom_smooth(method = lm, 
    se = TRUE) + ggtitle("Fiber Strength Data") + theme(text = element_text(size = 20)) + 
    xlab("Water Pressure (bars)") + ylab("Tensile Strength (N/5 cm)")

m1 <- lm(tensile ~ pressure, data = fiber, x = TRUE)
PE <- pureErrorAnova(m1)
rbind(reg.anova(m1)[1, ], tail(PE, 3), reg.anova(m1)[3, ])



