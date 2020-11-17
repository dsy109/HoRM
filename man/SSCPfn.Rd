\name{SSCP.fn}
\title{Sums of Squares and Cross-Products Matrices for a MANOVA Table}
\alias{SSCP.fn}
\usage{
SSCP.fn(fits)
}
\description{
Summarizes the MANOVA results based on the sum of squares and cross-products decomposition for the regression (SSCPR), the error (SSCPE), and the overall total (SSCPTO).
}

\arguments{
  \item{fits}{An object of class \code{manova}.}
} \value{
  \code{SSCP.fn} returns a list of length 3 with the SSCPR, SSCPE, and SSCPTO.
} \seealso{
  \code{\link{manova}}, \code{\link{reg.manova}}
} 


\references{
  Johnson, R. A. and Wichern, D. W. (2007), \emph{Applied Multivariate Statistical Analysis, Sixth Edition}, Pearson.

  Young, D. S. (2017), \emph{Handbook of Regression Methods}, CRC Press.
} 

\examples{ 
## Applied to the amit dataset.

data(amit)

fits <- manova(cbind(TOT, AMI) ~ ., data = amit)
SSCP.fn(fits = fits)

}

\keyword{file}


