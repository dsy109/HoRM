\name{hildreth.lu}
\title{Hildreth-Lu Procedure}
\alias{hildreth.lu}
\usage{
hildreth.lu(y, x, rho)
}
\description{
  Returns the linear regression fit for a given level of \code{rho} using the Hildreth-Lu procedure.
}

\arguments{
  \item{y}{A vector of response values.}
  \item{x}{A vector of predictor values. Must be the same length as \code{y}.}
  \item{rho}{A value for the correlation assumed for the autoregressive structure of the errors.}
} \value{
  \code{hildreth.lu} returns an object of class \code{lm} using the transformed quantities calculated for the Hildreth-Lu procedure.
} \seealso{
  \code{\link{lm}}
} 


\references{
  Hildreth, C. and Lu, J. Y. (1960), Demand Relations with Autocorrelated Disturbances, \emph{Technical Bulletin 276}, Michigan State University Agricultural Experiment Station.
  
  Young, D. S. (2017), \emph{Handbook of Regression Methods}, CRC Press.
} 

\examples{ 
## Example using the natural gas dataset.

data(gas)

out.1 <- hildreth.lu(y = gas$OK, x = gas$LA, rho = 0.1)
out.2 <- hildreth.lu(y = gas$OK, x = gas$LA, rho = 0.5)

out.1
out.2

}

\keyword{file}


