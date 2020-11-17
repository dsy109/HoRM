GOF.tests<- function(out){
  P <- sum(residuals(out,type="pearson")^2)
  P.test <- 1-pchisq(P,out$df.residual)
  G <- sum(residuals(out,type="deviance")^2)
  G.test <- 1-pchisq(G,out$df.residual)
  tmp <- data.frame(Statistic=c(P,G),p.value=c(P.test,G.test))
  rownames(tmp) <- c("Pearson","Deviance")
  tmp
}
