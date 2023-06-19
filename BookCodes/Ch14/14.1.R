library(ggplot2)
library(leaps)
library(fastR)
library(DAAG)
library(faraway)
library(qpcR)

###############################################################################
### Example 14.6.1: Punting Data, cont'd
###############################################################################

data(punting, package = "faraway")

out.all <- regsubsets(Hang ~ . - Distance, data = punting, nbest = 1, method = "exhaustive")
out.back <- regsubsets(Hang ~ . - Distance, data = punting, nbest = 1, method = "backward")
out.for <- regsubsets(Hang ~ . - Distance, data = punting, nbest = 1, method = "forward")
out.seq <- regsubsets(Hang ~ . - Distance, data = punting, nbest = 1, method = "seqrep")

plot(out.all, scale = "bic")
plot(out.seq, scale = "bic")

summary(out.all)$which[, -1]
round(sqrt(summary(out.all)$rss/c(11:7)), 3)
summary(out.all)$rsq
summary(out.all)$adjr2
summary(out.all)$cp
print(summary(out.all)$bic, digits = 5)

summary(out.seq)$which[, -1]
round(sqrt(summary(out.seq)$rss/c(11:7)), 3)
summary(out.seq)$rsq
summary(out.seq)$adjr2
summary(out.seq)$cp
print(summary(out.seq)$bic, digits = 5)

full <- lm(Hang ~ . - Distance, data = punting)
out.step <- step(full, direction = "both")  # Stepwise regression

fit1 <- lm(Hang ~ LStr, data = punting)
fit2 <- lm(Hang ~ LStr + OStr, data = punting)
fit3 <- lm(Hang ~ LStr + LFlex + OStr, data = punting)

PRESS(fit1)
PRESS(fit2)
PRESS(fit3)

summary(fit2)

all.cv1 <- all.cv2 <- all.cv3 <- NULL

for (i in 2:13)
{
    CV1 <- cv.lm(punting, fit1, m = i, plotit = F)
    all.cv1 <- c(all.cv1, mean((CV1$Hang - CV1$cvpred)^2))
    CV2 <- cv.lm(punting, fit2, m = i, plotit = F)
    all.cv2 <- c(all.cv2, mean((CV2$Hang - CV2$cvpred)^2))
    CV3 <- cv.lm(punting, fit3, m = i, plotit = F)
    all.cv3 <- c(all.cv3, mean((CV3$Hang - CV3$cvpred)^2))
}

modelname <- c(rep("Model 1", 13), rep("Model 2", 13), rep("Model 3", 13))
pressres <- data.frame(res = rbind(matrix(PRESS(fit1)$residuals), matrix(PRESS(fit2)$residuals), 
    matrix(PRESS(fit3)$residuals)))
pressres <- cbind(obs = seq(1, 13, 1), pressres, modelname)
ggplot(pressres, aes(x = obs, y = res, color = modelname, shape = modelname)) + geom_point(size = 3) + 
    xlab("Observation Number") + ylab("PRESS Residuals") + ggtitle("PRESS Residuals vs. Observation Number") + 
    theme(text = element_text(size = 20)) + labs(color = "Model", shape = "Model")

all <- data.frame(rbind(cv1 = matrix(all.cv1), cv2 = matrix(all.cv2), cv3 = matrix(all.cv3)))
modeltype <- c(rep("Model 1", 12), rep("Model 2", 12), rep("Model 3", 12))
k <- seq(2, 13, 1)
all <- cbind(k, all, modeltype)
colnames(all) <- c("K", "res", "modeltype")
ggplot(all, aes(x = K, y = res, col = modeltype, shape = modeltype)) + geom_point(size = 3) + 
    geom_line() + ggtitle("CV Estimate of Prediction Error") + ylab("CV Estimate of Prediction Error") + 
    theme(text = element_text(size = 20)) + labs(color = "Model", shape = "Model")
