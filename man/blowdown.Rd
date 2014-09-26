\name{Blowdown}
\alias{Blowdown}
\title{Blowdown data}
\description{
Data from the Boundary Waters Canoe Area Wilderness Blowdown.  The data frame blowdown
includes nine species of trees.  The data for balsam fir, summarized by diameter
class, are given in \code{BlowBF}.
}
\format{This data frame contains the following columns:
 \describe{
    \item{d}{
Tree diameter, in cm
    }
    \item{s}{
 Proportion of basal area killed for the four species balsam fir, cedar, 
 paper birch and blue spruse, a measure of
 local severity of the storm.
    }
    \item{spp}{Tree species, a factor with 9 levels
    }
    \item{y}{
 1 if the tree died, 0 if it survived
    }

 }
}
\source{Roy Rich}
\examples{
head(Blowdown)
}
\references{
S. Weisberg (2014).  \emph{Applied Linear Regression}, fourth edition.  New York:  Wiley.
}
\keyword{datasets}
