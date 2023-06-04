library(ggplot2)
library(HoRM)
library(conjoint)
library(aprean3)
library(car)
library(MASS)

###############################################################################
### Example 9.6.3: Tea Data
###############################################################################

data(tea, package = "conjoint")
caModel(y = tprefm[1, ], x = tprof)

y <- as.numeric(tprefm[1, ])
x <- data.frame(factor(tprof[, 1]), factor(tprof[, 2]), factor(tprof[, 3]), factor(tprof[, 
    4]))
names(x) <- names(tprof)
df <- data.frame(y, x)
ind <- lm(y ~ ., data = df)
summary(ind)

y2 <- NULL
for (i in 1:100) y2 <- c(y2, as.numeric(tprefm[i, ]))
x2 <- NULL
for (i in 1:100) x2 <- rbind(x2, x)
df2 <- data.frame(y2, x2)
full <- lm(y2 ~ ., data = df2)
summary(full)

