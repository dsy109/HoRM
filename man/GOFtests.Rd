\name{GOF.tests}
\title{Summary of Goodness-of-Fit Tests}
\alias{GOF.tests}
\usage{
GOF.tests(out)
}
\description{
  A function that reports the Pearson statistic, the deviance statistic, and their respective p-values for goodness-of-fit testing based on a linear regression fit (\code{lm}) or a generalized linear regression fit (\code{glm}).
}

\arguments{
  \item{out}{An object of class \code{lm} or \code{glm}.}
} \value{
  \code{GOF.tests} returns a data frame with rows corresponding to the goodness-of-fit test and columns corresponding to the respective test statistic and p-value.
} \seealso{
  \code{\link{glm}}, \code{\link{lm}}
} 


\references{
  Young, D. S. (2017), \emph{Handbook of Regression Methods}, CRC Press.
} 

\examples{ 
## Goodness-of-fit tests for the logistic regression fit to the
## menarche dataset.

data(menarche, package = "MASS")

glm.out = glm(cbind(Menarche, Total - Menarche) ~ Age, 
              family = binomial, data = menarche)
GOF.tests(glm.out)

}

\keyword{file}


