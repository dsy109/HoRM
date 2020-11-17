ppr_funs <- function(obj) {
  p <- obj$p
  q <- obj$q
  sm <- obj$smod
  n <- sm[4L]
  mu <- sm[5L]
  m <- sm[1L]
  jf <- q + 6 + m * (p + q)
  jt <- jf + m * n
  f <- matrix(sm[jf + 1L:(mu * n)], n, mu)
  t <- matrix(sm[jt + 1L:(mu * n)], n, mu)
  list(x = t, y = f)
}