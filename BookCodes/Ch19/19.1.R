library(ggplot2)
library(HoRM)
library(nlstools)

###############################################################################
### Example 19.4.1: Puromycin Data
###############################################################################

data(Puromycin, package = "datasets")
pur2 <- Puromycin[which(Puromycin$state == "treated"), ]
names(pur2)[1:2] <- c("S", "v")
out <- nls(michaelis, pur2, list(Km = 1, Vmax = 100))
summary(out)
plotfit(out, smooth = TRUE, pch.obs = 19, ylab = "Rate", xlab = "Concentration")
overview(out)

df1 <- data.frame(S = seq(0, max(pur2$S), length.out = 100))
df1$v <- predict(out, newdata = df1)
ggplot(pur2, aes(x = S, y = v)) + geom_point(size = 3) + geom_line(data = df1, aes(x = S, 
    y = v), col = 2) + ggtitle("Puromycin Data") + theme(text = element_text(size = 20)) + 
    ylab("Rate") + xlab("Concentration")

