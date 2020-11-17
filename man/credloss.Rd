\name{credloss}
\docType{data}
\alias{credloss}
\title{Credit Loss Dataset}
\usage{
data(credloss)
}
\description{
  This dataset consists of credit portfolio loss data that were extracted from the Altman-NYU Salomon Center Corporate Bond Default Database for the years 1982 through 2005.
}
\format{This data frame consists of 5 variables over 24 years:
\itemize{
  \item{\code{year}}{ The year the statistics were collected.}
  \item{\code{PD}}{ The probability of default.}
  \item{\code{defs}}{ The number of defaults.}
  \item{\code{LGD.mean}}{ The mean loss given default.}
  \item{\code{LGD.vol}}{ A loss given default volatility measure.}
}
}
\source{Bruche, M. and Gonzalez-Aguado, C. (2010), Recovery Rates, Default Probabilities, and the Credit Cycle, \emph{Journal of Banking and Finance},
  \bold{34}, 754--764.
}
\references{
  Young, D. S. (2017), \emph{Handbook of Regression Methods}, CRC Press.
}
\keyword{datasets}
