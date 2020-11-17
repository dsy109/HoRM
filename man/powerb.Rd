\name{power.b}
\title{Power Functions for Tests of Simple Linear Regression Coefficients}
\alias{power.b}
\usage{
power.b(x, y, alpha = 0.05, B0 = 0, B1 = 0)
}
\description{
  A function to calculate the power of the t-tests corresponding to tests on the intercept and slope coefficients in the simple linear regression model.
}

\arguments{
  \item{x}{A vector of predictor values. Must be the same length as \code{y}.}
  \item{y}{A vector of response values.  Must be the same length as \code{x}.}
  \item{alpha}{Significance level of the test.  Default level is 0.05.}
  \item{B0}{Null value for the test about the intercept.}
  \item{B1}{Null value for the test about the slope.}
} \value{
  \code{power.b} returns a matrix with the noncentrality parameters and power levels for the corresponding t-tests.
} \seealso{
  \code{\link{power.F}}
} 


\references{
  Young, D. S. (2017), \emph{Handbook of Regression Methods}, CRC Press.
} 

\examples{ 
## Applied to the toy dataset.

data(toy)

power.b(x = toy$x, y = toy$y)
}

\keyword{file}


