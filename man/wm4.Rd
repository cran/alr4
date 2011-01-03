\name{wm4}
\alias{wm4}
\docType{data}
\title{Windmill data with direction information and four reference sites}
\description{
Windspeed data collected at a test site for a windmill, and also at four nearby long-term weather
site, in Northern South Dakota.  
Data collected every six hours for all of 2002, except that all of the month
of May and a few other observations are missing.
}
\format{
  A data frame with 1114 observations on the following 14 variables.
  \describe{
    \item{Date}{A text variable with values like "2002/1/2/6" meaning the reading at 6AM on
    January 2, 2002}
    \item{CSpd}{Wind speed (m/s) at candidate site}
    \item{Spd1}{Wind speed (m/s) at reference site 1}
    \item{Spd2}{Wind speed (m/s) at reference site 2}
    \item{Spd3}{Wind speed (m/s) at reference site 3}
    \item{Spd4}{Wind speed (m/s) at reference site 4}
    \item{Spd1Lag1}{Lag 1 Wind speed (m/s) at reference site 1}
    \item{Spd2Lag1}{Lag 1 Wind speed (m/s) at reference site 2}
    \item{Spd3Lag1}{Lag 1 Wind speed (m/s) at reference site 3}
    \item{Spd4Lag1}{Lag 1 Wind speed (m/s) at reference site 4}
    \item{Spd1sin1}{Spd times sin(direction) at site 1}
    \item{Spd1cos1}{Spd times cos(direction) at site 1}
    \item{bin1}{Wind direction bin for site 1}
    \item{Dir1}{Wind direction for site 1, in degrees}
  }
}
\source{
Mark Ahlstrom and Rolf Miller, WindLogics, Inc.
}
\references{Weisberg, S. (in press). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(wm4)
}
\keyword{datasets}
