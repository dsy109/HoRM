hildreth.lu <- function(y, x, rho)
{
  if(abs(rho)>1) stop(paste("rho must be between -1 and 1!", 
                            "\n"))
  t <- 2:length(y)
  y <- y[t] - rho * y[t-1]
  x <- x[t] - rho * x[t-1]
  return(lm(y ~ x))
}
