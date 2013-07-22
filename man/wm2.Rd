\name{wm2}
\alias{wm2}
\docType{data}
\title{Windmill data with direction information}
\description{
 Windspeed data collected at a test site for a windmill, and also at a nearby long-term weather
site, in Northern South Dakota.  
Data collected every six hours for all of 2002, except that all of the month
of May and a few other observations missing.
}
\format{
  A data frame with 1116 observations on the following 5 variables.
  \describe{
    \item{Date}{A text variable with values like "2002/1/2/6" meaning the reading at 6AM on
    January 2, 2002}
    \item{CSpd}{Windspeed in m/s at the candidate site}
    \item{RSpd}{Windspeed for the reference site}
    \item{RDir}{Wind direction, in degrees, at the reference site}
    \item{Bin}{Wind direction binned into 16  equal width  bins}
  }
}
\source{
Mark Ahlstrom and Rolf Miller, WindLogics, Inc.
}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(wm2)
}
\keyword{datasets}
