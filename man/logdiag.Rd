\name{logdiag}
\title{Diagnostic Measures of Certain Regression Estimates}
\alias{logdiag}
\usage{
logdiag(out)
}
\description{
  A function for computing various residual-based and influence-based quantities from a linear regression fit using \code{lm} or a generalized linear regression fit using \code{glm}.
}

\arguments{
  \item{out}{An object of class \code{lm} or \code{glm}.}
} \value{
  \code{logdiag} returns a data frame with the following columns:
  \item{r.i}{The raw residuals.}
  \item{p.i}{The Pearson residuals.}
  \item{d.i}{The deviance residuals.}
  \item{stud.r.i}{The Studentized raw residuals.}
  \item{stud.p.i}{The Studentized Pearson residuals.}
  \item{stud.d.i}{The Studentized deviance residuals.}
  \item{h.ii}{The leverage values.}
  \item{C.i}{The Cook's distance value.}
  \item{C.i.bar}{The average Cook's distance value when omitting observation i.}
  \item{DFDEV}{The change in the deviance statistic when omitting observation i.}
  \item{DFCHI}{The change in the Pearson's chi-square statistic when omitting observation i.}
  \item{fit}{The estimated response (fitted) values.}
} \seealso{
  \code{\link{glm}}, \code{\link{lm}}
} 


\references{
  Young, D. S. (2017), \emph{Handbook of Regression Methods}, CRC Press.
} 

\examples{ 
## Diagnostic summaries for the logistic regression fit to the
## menarche dataset.

data(menarche, package = "MASS")

glm.out = glm(cbind(Menarche, Total - Menarche) ~ Age, 
              family = binomial, data = menarche)
logdiag(glm.out)

}

\keyword{file}


