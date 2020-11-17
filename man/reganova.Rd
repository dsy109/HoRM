\name{reg.anova}
\title{Expanded ANOVA Table}
\alias{reg.anova}
\usage{
reg.anova(lm.out)
}
\description{
  Calculate the ANOVA table for an object of class \code{lm}.  The results are identical to those obtained from \code{anova}, but an extra line is included that prints the total degrees of freedom and the total sum of squares.
}

\arguments{
  \item{lm.out}{An object of class \code{lm} (i.e., the results from the linear model fitting routine such that
  the \code{anova} function can act upon).}
} \value{
  \code{reg.anova} returns exactly the same output as the \code{anova} function applied to an object of class \code{lm}, but includes an extra line that summarizes the total source of variability.
} \seealso{
  \code{\link{anova}}, \code{\link{lm}}
} 


\references{
  Young, D. S. (2017), \emph{Handbook of Regression Methods}, CRC Press.
} 

\examples{ 
## Applied to the toy dataset.

data(toy)

lm.out <- lm(y ~ x, data = toy)
anova(lm.out)
reg.anova(lm.out)
}

\keyword{file}


