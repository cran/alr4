\name{BlowBS}
\alias{BlowBS}
\title{Blowdown data, Black Spruce only}
\description{
Data from the Boundary Waters Canoe Area Wilderness Blowdown.  The data
frame \code{Blowdown}
includes nine species of trees, but this file only includes black spruce, grouped
by diameter.
}
\format{This data frame contains the following columns:
 \describe{
    \item{d}{
Tree diameter, in cm
    }
    \item{died}{
Number of trees of this value of \code{d} that died (blowdown)
    }
    \item{m}{
number of trees of this size class measured
    }
 }
}
\source{Roy Rich}
\examples{
head(BlowBS)
}
\references{
S. Weisberg (2014).  \emph{Applied Linear Regression}, fourth edition.  New York:  Wiley.
}
\keyword{datasets}
