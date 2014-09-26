\name{domedata}
\alias{domedata}
\alias{domedata1}
\docType{data}
\title{Metrodome fan experiment}
\description{
These files give the results of two experiments to see if manipulating the air conditioning fans
in the Minneapolis metrodome can effect the distance travelled by a baseball.  The data in domedata
were collected in April 2003.  The experiment was repeated in May 2003 and domedata1 gives the
combined data from the two experiments.
}
\format{
  A data frame with 96 observations on the following 7 variables.
  \describe{
    \item{Date}{a factor with levels \code{March}- \code{May}}
    \item{Cond}{a factor with levels \code{Headwind}, \code{Tailwind}}
    \item{Angle}{the actual angle}
    \item{Velocity}{in feet per second}
    \item{BallWt}{weight of ball in grams}
    \item{BallDia}{diameter of ball in inches}
    \item{Dist}{distance in feet of the flight of the ball}
  }
}

\source{
 Ivan Marusic
}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(domedata1)
}
\keyword{datasets}
