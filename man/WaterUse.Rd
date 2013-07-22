\name{MinnWater}
\alias{MinnWater}
\docType{data}
\title{
Total Water Usage in Minnesota
}
\description{
Yearly water consumption in Minnesota from 1988-2011.
}
\usage{data(MinnWater)}
\format{
  A data frame with 24 observations on the following variables.
  \describe{
    \item{year}{year}
    \item{allUse}{total ground water consumption, statewide, in billions
of gallons}
    \item{muniUse}{total municipal water consumption, statewide, in billions
of gallons}
    \item{irrUse}{consumption for irrigation in 13 counties, in billions
of gallons}
    \item{agPrecip}{average growing season June to August precipiciation (inches) for the 13
Minnesota counties that use the most irrigation}
    \item{muniPrecip}{average May to September precipiciation (inches) for the 10
Minnesota counties with highest municipal water pumping}
    \item{statePop}{estimated state population}
    \item{muniPop}{estimated 10 county urban population}
  }
}
\details{
Is water usage increasing?  How fast?
}
\source{
These data were provided by the Freshwater Society.  They collected the data
from the Minnesota Department of Natural Resources and from the Minnesota
Climatology Working Group.  Thanks to Tom Burk.
}

\examples{
data(MinnWater)
## maybe str(MinnWater) ; plot(MinnWater) ...
}
\keyword{datasets}
