\name{poly2form}
\title{Expands Design Matrix Based on Polynomials}
\alias{poly2form}
\usage{
poly2form(poly.out, x)
}
\description{
  This function takes a list of objects having class \code{polynomial}, evaluates each polynomial as a function of \code{x}, then returns the results in a matrix.
}

\arguments{
  \item{poly.out}{A list whose objects are of class \code{polynomial}.}
  \item{x}{A vector of values for which each polynomial in \code{poly.out} is to be evaluated.}
} \value{
  \code{poly.out} returns a matrix whose columns are the evaluation of each polynomial in \code{poly.out} using \code{x}.
} \seealso{
  \code{\link{legendre.polynomials}}
} 


\references{
  Young, D. S. (2017), \emph{Handbook of Regression Methods}, CRC Press.
} 

\examples{ 
## Evaluating the order 5 Legendre polynomials.

require(orthopolynom)

px <- legendre.polynomials(n = 5, normalized = FALSE)
lx <- poly2form(poly.out = px, x = 1:10)

lx

}

\keyword{file}


