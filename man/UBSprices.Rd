\name{UBSprices}
\alias{UBSprices}
\docType{data}
\title{
UBS price data
}
\description{
The international bank UBS produces a report on prices in major world cities
every three years.  This data.frame includes price data for a 1 kg loaf of
bread, 1 kg of rice and for a Big Mac hamburger, for the years 2003 and 2009.
All these prices are measured in the minutes of labor required by the typical
worker in that country to buy the product, so it adjusts for currency, wages
and price levels.
}
\usage{data(UBSprices)}
\format{
  A data frame with 54 observations on the following 6 variables.
  \describe{
    \item{\code{bigmac2009}}{2009 Big Mac price, in minutes of labor}
    \item{\code{bread2009}}{2009 Bread price, in minutes of labor}
    \item{\code{rice2009}}{2009 Rice price, in minutes of labor}
    \item{\code{bigmac2003}}{2003 Big Mac price, in minutes of labor}
    \item{\code{bread2003}}{2003 Bread price, in minutes of labor}
    \item{\code{rice2003}}{2003 Rice price, in minutes of labor}
  }
}
\details{
City names are the row labels.
}
\source{
\url{http://www.ubs.com/global/en/wealth_management/wealth_management_research/prices_earnings.html}
}
\examples{
data(UBSprices)
## maybe str(UBSprices) ; plot(UBSprices) ...
}
\keyword{datasets}
