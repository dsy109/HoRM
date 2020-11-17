power.b <- function(x,y,alpha=0.05,B0=0,B1=0){
  out <- lm(y~x)
  MSE <- (summary(out)$sigma)^2
  delta <- c(coef(out)-c(B0,B1))^2*sum((x-mean(x))^2)/MSE
  p.1 <- pf(qf(1-alpha,df1=1,df2=length(x)-2),df1=1,df2=length(x)-2,ncp=delta[1],lower.tail=FALSE)
  p.2 <- pf(qf(1-alpha,df1=1,df2=length(x)-2),df1=1,df2=length(x)-2,ncp=delta[2],lower.tail=FALSE)
  DF <- data.frame(ncp=delta,power=c(p.1,p.2))
  rownames(DF) <- c("Intercept","Slope")
  return(DF)
}
