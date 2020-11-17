logdiag <- function(out){
  raw.res <- residuals(out,type="response")
  prs.res <- residuals(out,type="pearson")
  dev.res <- residuals(out,type="deviance")
  raw.res.stud <- rstudent(out,type="response")
  prs.res.stud <- rstudent(out,type="pearson")
  dev.res.stud <- rstudent(out,type="deviance")
  h.ii <- hatvalues(out)
  Ci.bar <- prs.res^2*h.ii/(1-h.ii)
  Ci <- Ci.bar/(1-h.ii)
  DFDEV <- dev.res+Ci.bar
  DFCHI <- Ci.bar/h.ii
  ret <- data.frame(r.i=raw.res,p.i=prs.res,d.i=dev.res,
                    stud.r.i=raw.res.stud,stud.p.i=prs.res.stud,stud.d.i=dev.res.stud,
                    h.ii=h.ii,C.i=Ci,C.i.bar=Ci.bar,DFDEV=DFDEV,DFCHI=DFCHI,fit=out$fitted.values)
  ret
}