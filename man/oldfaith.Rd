\name{oldfaith}
\alias{oldfaith}
\docType{data}
\title{Old Faithful Geyser data}
\description{
Data on eruptions of Old Faithful Geyser, October 1980.  Variables are the
duration in seconds of the current eruption, and the time in minutes to the
next eruption.  Collected by volunteers, and supplied by the Yellowstone
National Park Geologist.  Data was not collected between approximately
midnight and 6 AM.
}
\format{This data frame contains the following columns:
 \describe{
    \item{Duration}{
 Duration in seconds
    }
    \item{Interval}{
 Time to next eruption
    }
 }
}

\source{R. Hutchinson}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(oldfaith)
}
\keyword{datasets}
