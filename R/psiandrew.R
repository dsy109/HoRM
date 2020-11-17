psi.andrew <- function(u, k=1.339, deriv=0){
  if(!deriv) ifelse(u < pi*k, sin(u/k)/(u/k), 0)
  abs(u) < pi*k
}