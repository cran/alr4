\name{challeng}
\alias{challeng}
\docType{data}
\title{Challenger data}
\description{
Contains data from the performance of O-rings in 23 U.S. space shuttle flights prior to the Challenger disaster of January 20, 1986.
}
\format{This data frame contains the following columns:
 \describe{
    \item{Temp}{
 Air Temp at launch (degrees F)
    }
    \item{Pres}{
 Leak check pressure
    }
    \item{Fail}{
 Number of O-rings that failed
    }
    \item{n}{
 6, number of O-rings in launch
    }
    \item{Erosion}{
 Number of erosion incidents
    }
    \item{BlowBy}{
 Number of blowby incidents
    }
    \item{Damage}{
 Total Damage Index
    }
    \item{Date}{
 date of flight
    }
 }
}

\source{
Dalal, S, Fowlkes, E. B. and Hoadley, B. (1989),
Risk analysis of the space shuttle:  Pre-challenger prediction of
failure, \emph{Journal of the American Statistical Association}, 84, 945-957.
See also Tufte, E. R. (1997), \emph{Visual and statistical Thinking:  Displays of
evidence for making decisions}, Cheshire, CT:  Graphics Press.}
\references{Weisberg, S. (in press). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(challeng)
}
\keyword{datasets}
