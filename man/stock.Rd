\name{stock}
\docType{data}
\alias{stock}
\title{Google Stock Dataset}
\usage{
data(stock)
}
\description{
  This dataset consists of the closing stock price of a share of Google stock during the trading days between February 7-th and July 7-th of 2005.
}
\format{This is an extensible time series (\code{xts}) object for the 105 trading days of interest:
\itemize{
  \item{\code{GOOG.close}}{ The closing stock price of a share of Google stock.}
}
}
\source{Yahoo! Finance; accessed 01-26-2017.
}
\references{
  Young, D. S. (2017), \emph{Handbook of Regression Methods}, CRC Press.
}
\examples{ 
\dontrun{
## How the data were accessed (1/26/17).

require(quantmod)

getSymbols("GOOG", src = "yahoo", 
           from = "2005-02-07", to = "2005-07-07")
stock <- GOOG[,4]
}
}
\keyword{datasets}
