\name{Challeng}
\alias{Challeng}
\docType{data}
\title{Challenger data}
\description{
Contains data from the performance of O-rings in 23 U.S. space shuttle flights prior to the Challenger disaster of January 20, 1986.
}
\format{This data frame uses dates as row names and contains the following columns:
 \describe{
    \item{temp}{
 Air Temp at launch (degrees F)
    }
    \item{pres}{
 Leak check pressure
    }
    \item{fail}{
 Number of O-rings that failed
    }
    \item{n}{
 6, number of O-rings in launch
    }
    \item{erosion}{
 Number of erosion incidents
    }
    \item{blowby}{
 Number of blowby incidents
    }
    \item{damage}{
 Total Damage Index
    }
 }
}

\source{
Dalal, S, Fowlkes, E. B. and Hoadley, B. (1989),
Risk analysis of the space shuttle:  Pre-challenger prediction of
failure, \emph{Journal of the American Statistical Association}, 84, 945-957.
See also Tufte, E. R. (1997), \emph{Visual and statistical Thinking:  Displays of
evidence for making decisions}, Cheshire, CT:  Graphics Press.}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(Challeng)
}
\keyword{datasets}
