\name{segreg}
\alias{segreg}
\docType{data}
\title{Energy consumption}
\description{
Data on electricity consumption (KWH) and mean temperature (degrees F) for one
building on the University of Minnesota's Twin Cities campus.
for 39 months in 1988-92.  The goal is to model consumption
as a function of temperature.  Higher temperature causes the use of air
conditioning, so high temperatures should mean high consumption.
This building is steam heated, so electricity is not used for heating.
}
\format{This data frame contains the following columns:
 \describe{
    \item{Temp}{
 Monthly mean temperature, degrees F.
    }
    \item{C}{
 Electricty consumption in KWH/day
    }
 }
}

\source{Charles Ng}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(segreg)
}
\keyword{datasets}
