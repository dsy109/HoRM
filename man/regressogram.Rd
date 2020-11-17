\name{regressogram}
\title{Regressogram}
\alias{regressogram}
\usage{
regressogram(x, y, nbins = 10, show.bins = TRUE,
			       show.means = TRUE, show.lines = TRUE,
             x.lab = "X", y.lab = "Y", main = "TITLE")
}
\description{
  Computes and plots the regressogram for a single predictor and single response relationship.   The regressogram is plotted using \code{ggplot2}.
}

\arguments{
  \item{x}{A vector of predictor values for the data.  Must be the same length as \code{y}.}
  \item{y}{A vector of response values for the data.  Must be the same length as \code{x}.}
  \item{nbins}{How many bins to use construction of the regressogram.}
  \item{show.bins}{A logical argument specifying if dashed vertical lines should be drawn at the boundaries of the bins.  Default is \code{TRUE}.}
  \item{show.means}{A logical argument specifying if a large point should be overlayed at the midpoint of each bin and the respective mean of the response values within that bin.  Default is \code{TRUE}.}
  \item{show.lines}{A logical argument specifying if a line should be drawn connecting the points determined by \code{show.means}.  Default is \code{TRUE}.}
  \item{x.lab}{Label for the x-axis.}
  \item{y.lab}{Label for the y-axis.}
  \item{main}{Title for the regressogram.}
} \value{
  \code{regressogram} returns a plotted regressogram using the \code{ggplot2} package.
} \seealso{
  \code{\link{hist}}
} 


\references{
  Wasserman, L. (2006), \emph{All of Nonparametric Statistics}, Springer.

  Young, D. S. (2017), \emph{Handbook of Regression Methods}, CRC Press.
} 

\examples{ 
## Regressogram for the natural gas dataset.

data(gas)

regressogram(x = gas$LA, y = gas$OK, nbins = 6, x.lab = "LA",
             y.lab = "OK", main = "Regressogram")

}

\keyword{file}


