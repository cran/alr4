\name{npdata}
\alias{npdata}
\docType{data}
\title{Northern pike catch per unit effort}
\description{
 Catch per unit effort data for 16 Minnesota lakes
}
\format{
  A data frame with 16 observations on the following 4 variables.
  \describe{
    \item{CPUE}{Estimated catch per unit effect}
    \item{SECPUE}{Estimated standard error of CPUE}
    \item{Density}{Estimated fish density}
    \item{SEdens}{Estimated standard error of Density}
  }
}

\source{
R. Pierce, Minnesota Dept. of Natural Resources
}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(npdata)
}
\keyword{datasets}
