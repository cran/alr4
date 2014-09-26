\name{Hooker}
\alias{Hooker}
\docType{data}
\title{Hooker's data}
\description{
In his original paper, Forbes provided additional data collected by the botanist Dr. Joseph Hooker on temperatures and boiling points measured often at higher altitudes in the Himalaya Mountains.
}
\format{This data frame contains the following columns:
 \describe{
    \item{bp}{
Measured boiling temperature, degrees F. 
    }
    \item{pres}{
Measured air pressure, inches of Mercury. 
    }
    \item{lpres}{
100 times pres rounded to two decimals.
    }
 }
}

\source{Forbes, J. (1957). Further experiments and remarks on the measurement of heights by boiling point of water. \emph{Transactions of the Royal Society of Edinburgh}, 21, 235-243.}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(Hooker)
}
\keyword{datasets}
