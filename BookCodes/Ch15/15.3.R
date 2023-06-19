library(ggplot2)
library(HoRM)
library(RColorBrewer)
library(lme4)
library(car)
library(rsm)
library(daewr)
library(mixexp)

###############################################################################
### Example 15.5.3: Odor Data
###############################################################################

data(chem, package = "HoRM")
print(chem, decode = FALSE)

chem.fit <- rsm(odor ~ FO(temp.c, ratio.c, height.c) + PQ(temp.c, ratio.c, height.c), 
    data = chem)
summary(chem.fit)

chem.fit2 <- rsm(odor ~ FO(temp.c, ratio.c, height.c) + I(temp.c^2) + I(ratio.c^2), 
    data = chem)
summary(chem.fit2)

temp.c <- seq(40, 120, length = 1000)
height.c <- seq(2, 6, length = 1000)
ratio.c <- seq(0.3, 0.7, length = 1000)

temp.ratio <- cbind(expand.grid(temp.c, ratio.c), 4)
colnames(temp.ratio) <- c("temp", "ratio", "height")
temp.ratio <- coded.data(temp.ratio, temp.c ~ (temp - 80)/40, ratio.c ~ (ratio - 
    0.5)/0.2, height.c ~ (height - 4)/2)
colnames(temp.ratio) <- c("temp.c", "ratio.c", "height.c")
temp.ratio <- cbind(temp.ratio, Odor = predict(chem.fit2, newdata = temp.ratio))

temp.height <- cbind(expand.grid(temp.c, height.c), 0.5)
colnames(temp.height) <- c("temp", "height", "ratio")
temp.height <- coded.data(temp.height, temp.c ~ (temp - 80)/40, ratio.c ~ (ratio - 
    0.5)/0.2, height.c ~ (height - 4)/2)
colnames(temp.height) <- c("temp.c", "height.c", "ratio.c")
temp.height <- cbind(temp.height, Odor = predict(chem.fit2, newdata = temp.height))

height.ratio <- cbind(expand.grid(height.c, ratio.c), 80)
colnames(height.ratio) <- c("height", "ratio", "temp")
height.ratio <- coded.data(height.ratio, temp.c ~ (temp - 80)/40, ratio.c ~ (ratio - 
    0.5)/0.2, height.c ~ (height - 4)/2)
colnames(height.ratio) <- c("height.c", "ratio.c", "temp.c")
height.ratio <- cbind(height.ratio, Odor = predict(chem.fit2, newdata = height.ratio))

ggplot(temp.ratio, aes(x = temp.c, y = ratio.c, z = Odor)) + geom_raster(aes(fill = Odor)) + 
    geom_contour(colour = "black") + scale_fill_distiller(palette = "Spectral") + 
    scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + 
    xlab("Temperature") + ylab("Ratio") + ggtitle("Odor Data (Height = 4)") + theme(text = element_text(size = 20))

ggplot(temp.height, aes(x = temp.c, y = height.c, z = Odor)) + geom_raster(aes(fill = Odor)) + 
    geom_contour(colour = "black") + scale_fill_distiller(palette = "Spectral") + 
    scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + 
    xlab("Temperature") + ylab("Height") + ggtitle("Odor Data (Ratio = 0.5)") + theme(text = element_text(size = 20))

ggplot(height.ratio, aes(x = height.c, y = ratio.c, z = Odor)) + geom_raster(aes(fill = Odor)) + 
    geom_contour(colour = "black") + scale_fill_distiller(palette = "Spectral") + 
    scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + 
    xlab("Height") + ylab("Ratio") + ggtitle("Odor Data (Temperature = 80)") + theme(text = element_text(size = 20))

ind <- round(seq(1, 1000, length = 100))

persp(temp.c[ind], ratio.c[ind], matrix(temp.ratio$Odor, nrow = 1000)[ind, ind], 
    col = rainbow(50), zlab = "Odor", xlab = "Temperature", ylab = "Ratio", theta = -30, 
    phi = 25)
mtext("Odor Data (Height = 4)", side = 3, adj = 0, line = 1.2, cex = 1.5, font = 2)

persp(temp.c[ind], height.c[ind], matrix(temp.height$Odor, nrow = 1000)[ind, ind], 
    col = rainbow(50), zlab = "Odor", xlab = "Temperature", ylab = "Height", theta = -30, 
    phi = 25)
mtext("Odor Data (Ratio = 0.5)", side = 3, adj = 0, line = 1.2, cex = 1.5, font = 2)

persp(height.c[ind], ratio.c[ind], matrix(height.ratio$Odor, nrow = 1000)[ind, ind], 
    col = rainbow(50), zlab = "Odor", xlab = "Height", ylab = "Ratio", theta = -30, 
    phi = 25)
mtext("Odor Data (Temperature = 80)", side = 3, adj = 0, line = 1.2, cex = 1.5, font = 2)

