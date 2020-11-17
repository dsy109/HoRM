\name{power.F}
\title{Power Function for the General Linear F-Test}
\alias{power.F}
\usage{
power.F(full, reduced, alpha = 0.05)
}
\description{
  A function to calculate the power of the general linear F-test.
}

\arguments{
  \item{full}{The full model (specified in the alternative hypothesis) in the general linear F-test. This is an object of class \code{lm}.}
  \item{reduced}{The reduced model (specified in the null hypothesis) in the general linear F-test. This is an object of class \code{lm}.}
  \item{alpha}{Significance level of the test.  Default level is 0.05.}
} \value{
  \code{power.F} returns a single value (saved as a matrix) with the power for the corresponding general linear F-test.
} \seealso{
  \code{\link{anova}}, \code{\link{lm}}
} 


\references{
  Young, D. S. (2017), \emph{Handbook of Regression Methods}, CRC Press.
} 

\examples{ 
## Applied to the toy dataset.

data(toy)

full <- lm(y~x, data = toy)
reduced <- lm(y~1, data = toy)
power.F(full = full, reduced = reduced, alpha = 0.05)
}

\keyword{file}


