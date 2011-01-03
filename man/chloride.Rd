\name{chloride}
\alias{chloride}
\docType{data}
\title{Chloride data}
\description{
Seasonal variation in chloride level in marshes close to a road or distant from the road, in Minnesota.
}
\format{
  A data frame with 32 observations on the following 4 variables.
  \describe{
    \item{Marsh}{March number}
    \item{Type}{a factor with levels \code{Isolated} and \code{Roadside}}
    \item{Month}{Month (4 = May, ... 10 = October)}
    \item{Cl}{Chloride concentration, mg/liter}
  }
}
\source{
Stefanie Miklovic and Susan Galatowitsch
}
\references{Weisberg, S. (in press). \emph{Applied Linnear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(chloride)
}
\keyword{datasets}
