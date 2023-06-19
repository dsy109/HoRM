library(ggplot2)
library(HoRM)
library(RColorBrewer)
library(lme4)
library(car)
library(rsm)
library(daewr)
library(mixexp)

###############################################################################
### Example 15.5.1: Sleep Study Data
###############################################################################

data(sleepstudy, package = "lme4")
p <- qplot(Days, Reaction, data = sleepstudy) + theme(text = element_text(size = 20)) + 
    geom_smooth(method = "lm", formula = y ~ x)
p + facet_wrap(~Subject, nrow = 3)

out.reml <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy, REML = TRUE)
out.logl <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy, REML = FALSE)
summary(out.reml)
summary(out.logl)

