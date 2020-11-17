SSCP.fn <- function(fits){
  SS <- summary.manova(fits)$"SS"
  p <- length(SS)-1
  SSCPR <- 0
  for(i in 1:p) SSCPR <- SSCPR + SS[[i]]
  out <- list(SSCPR=SSCPR,SSCPE=SS$Residuals,SSCPTO=SSCPR+SS$Residuals)
  out
}