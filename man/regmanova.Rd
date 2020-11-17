\name{reg.manova}
\title{Expanded MANOVA Table}
\alias{reg.manova}
\usage{
reg.manova(AOV.out)
}
\description{
  Expands the MANOVA results from an object of class \code{summary.aov}.  The results are identical to those obtained from \code{summary.aov}, but an extra line is included that prints the total degrees of freedom and the total sum of squares for each dimension of the response vector.
}

\arguments{
  \item{AOV.out}{An object of class \code{summary.aov}.}
} \value{
  \code{AOV.out} returns exactly the same output as the \code{summary.aov} function, but includes an extra line that summarizes the total source of variability for each dimension of the response vector.
} \seealso{
  \code{\link{anova}}, \code{\link{reg.anova}}, \code{\link{summary.aov}}
} 


\references{
  Young, D. S. (2017), \emph{Handbook of Regression Methods}, CRC Press.
} 

\examples{ 
## Applied to the amit dataset.

data(amit)

fits <- manova(cbind(TOT, AMI) ~ ., data = amit)
out <- summary.aov(fits)
mvreg.out <- lapply(out, reg.manova)
mvreg.out

}

\keyword{file}


