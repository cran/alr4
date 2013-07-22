\name{BlowBF}
\alias{BlowBF}
\title{Blowdown data, Balsam Fir only}
\description{
Data from the Boundary Waters Canoe Area Wilderness Blowdown.  The data frame blowdown
includes nine species of trees, but this file only includes paper birch, grouped
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
head(BlowBF)
}
\references{
S. Weisberg (2014).  \emph{Applied Linear Regression}, fourth edition.  New York:  Wiley.
}
\keyword{datasets}
