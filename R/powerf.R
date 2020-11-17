power.F <- function(full,reduced,alpha=0.05){
  F.test <- anova(reduced,full)
  n <- length(full$fitted.values)
  delta=n*(summary(full)$r.sq-summary(reduced)$r.sq)/(1-summary(full)$r.sq)
  A=1-pf(qf(1-alpha,F.test$Df[2],F.test$Res.Df[2]),F.test$Df[2],F.test$Res.Df[2],ncp=delta)
  TT=rbind(A)
  colnames(TT)="Power"
  rownames(TT)=c("F-Test")
  TT
}
