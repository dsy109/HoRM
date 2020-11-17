\name{psi.andrew}
\title{Andrew's Sine Function}
\alias{psi.andrew}
\usage{
psi.andrew(u, k=1.339, deriv=0)
}
\description{
  Andrew's sine function for use when fitting a linear model by robust regression using an M-estimator.
}

\arguments{
  \item{u}{Numeric vector of evaluation points.}
  \item{k}{Tuning constant.  The suggested default value is 1.339.}
  \item{deriv}{0 or 1: to compute values of this function or of its first derivative.}
} \value{
  \code{psi.andrew} returns a vector of points evaluated using Andrew's sine function.
} \seealso{
  \code{\link{rlm}}
} 


\references{
  Andrew, D. F. (1974), A Robust Method for Multiple Linear Regression, \emph{Technometrics},
  \bold{16}, 523--531.

  Young, D. S. (2017), \emph{Handbook of Regression Methods}, CRC Press.
} 

\examples{ 
## Robust fit of the stackloss dataset.

require(MASS)

data(stackloss, package="datasets")

out <- rlm(stack.loss ~ ., data = stackloss, 
           psi = psi.andrew)

out
}

\keyword{file}


