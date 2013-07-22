\name{lakemary}
\alias{lakemary}
\docType{data}
\title{Lake Mary bluegills}
\description{
78 bluegills were captured from Lake Mary, Minnesota.  On each fish, a
key scale was removed.  The age of a fish is determined by counting the
number of annular rings on the scale.  The goal is to relate length at
capture to the radius of the scale.  
}
\format{This data frame contains the following columns:
 \describe{
    \item{Age}{
 Years
    }
    \item{Length}{
 mm
    }
 }
}

\source{
Collected by Richard Frie, and discussed
in S. Weisberg (1986), A linear model approach to the backcalculation of
fish length, \emph{J. Amer. Statist. Assoc.}, 81, 922-929.}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(lakemary)
}
\keyword{datasets}
