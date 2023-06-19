library(ggplot2)
library(HoRM)
library(MethComp)
library(deming)
library(sem)

###############################################################################
### Example 11.6.3: Arsenate Assay Data
###############################################################################

data(arsenate, package = "deming")
attach(arsenate)

out.ls <- coef(lm(aes ~ aas))
out.Dem <- Deming(aas, aes, vr = 0.79)
out.Dem

ggplot(arsenate, aes(x = aas, y = aes)) + geom_point(size = 3) + ggtitle("Arsenate Assay Data") + 
    theme(text = element_text(size = 20)) + xlab("Atomic Absorption Spectrometry (ug/L)") + 
    ylab("Atomic Emission Spectroscopy (ug/L)") + geom_abline(aes(slope = out.ls[2], 
    intercept = out.ls[1], color = "OLS", linetype = "OLS"), size = 1.3) + geom_abline(aes(slope = out.Dem[2], 
    intercept = out.Dem[1], color = "Deming", linetype = "Deming"), size = 1.3) + 
    scale_colour_manual(name = "Regression Type", values = c("#E69F00", "#56B4E9")) + 
    scale_linetype_manual(name = "Regression Type", values = c("dashed", "dotted")) + 
    theme(legend.text = element_text(size = 10), legend.title = element_text(size = 10), 
        plot.title = element_text(size = 25), axis.text = element_text(size = 20), 
        axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20)) + 
    theme(legend.background = element_rect(color = "black", size = 0.1)) + theme(legend.key.width = unit(3, 
    "line"))

