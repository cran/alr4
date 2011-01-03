\name{forbes}
\alias{forbes}
\docType{data}
\title{Forbes data}
\description{
The data consists of 17 pairs of numbers corresponding
to observed boiling point and corrected barometric pressure, at locations in
the Alps.
}
\format{This data frame contains the following columns.  The first two columns
of this data frame are also given in the data file forbes in the
MASS package, with different column labels.
 \describe{
    \item{Temp}{Adjusted boiling point of water in degrees F.
 
    }
    \item{Pressure}{Atmospheric pressure, in inches of Mercury
 
    }
    \item{Lpres}{100 times log(Pressure,10), rounded to two digits beyond
the decimal point
 
    }
 }
}

\source{
Forbes, J. (1857). Further experiments and remarks on the measurement of heights and boiling point of water. \emph{Transactions of the Royal Society of Edinburgh}, 21, 235-243.}
\references{Weisberg, S. (in press). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(forbes)
}
\keyword{datasets}
