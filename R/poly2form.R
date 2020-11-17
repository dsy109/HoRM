poly2form <- function(poly.out,x){
  sapply(1:length(poly.out),function(i) as.function(poly.out[[i]])(x))
}
