\name{ppr_funs}
\title{Ridge Functions for Projection Pursuit Regression}
\alias{ppr_funs}
\usage{
ppr_funs(obj)
}
\description{
  The portion of the \code{plot.ppr} code that computes the ridge traces for projection pursuit regression.
}

\arguments{
  \item{obj}{A fit of class \code{ppr} as produced by the \code{ppr} function.}
} \value{
  \code{ppr_funs} returns the evaluated ridge trace values based on output from the \code{ppr} function.
} \seealso{
  \code{\link{ppr}}, \code{\link{plot.ppr}}
} 

\details{
	This is just the segment of code in \code{plot.ppr}, which calculates the ridge traces.
}

\references{
  Young, D. S. (2017), \emph{Handbook of Regression Methods}, CRC Press.
} 

\examples{ 
## Projection pursuit regression on the rock dataset.

data(rock)

ppr.out <- ppr(log(perm) ~ area + peri + shape,
               data = rock, nterms = 2, max.terms = 5)
obj <- ppr_funs(ppr.out)

obj

}

\keyword{file}


