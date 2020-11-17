reg.anova <- function(lm.out){
  AOV.out <- anova(lm.out)
  Residuals <- as.numeric(tail(AOV.out,1))
  Regression.df <- sum(head(AOV.out[,1],-1))
  Regression.SS <- sum(head(AOV.out[,2],-1))
  Regression.MS <- Regression.SS/Regression.df
  Regression.F <- Regression.MS/Residuals[3]
  Regression.p <- pf(Regression.F,Regression.df,Residuals[1],
                     lower.tail=FALSE)
  Regression <- c(Regression.df,Regression.SS,Regression.MS,
                  Regression.F,Regression.p)
  out <- rbind(Regression,AOV.out)[c(1,dim(AOV.out)[1]+1),]
  rownames(out)[1] <- "Regression"
  out <- rbind(out,c(sum(out[,1]),sum(out[,2]),rep("",ncol(out)-2)))
  rownames(out)[nrow(out)] <- "Total"
  out
}
