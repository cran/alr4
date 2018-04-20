\name{Forbes}
\alias{Forbes}
\docType{data}
\title{Forbes' data}
\description{
The data consists of 17 pairs of numbers corresponding
to observed boiling point and corrected barometric pressure, at locations in
the Alps.
}
\format{This data frame contains three columns.  The first two columns are
 identical to the data set named forbes in the MASS package.
 \describe{
    \item{bp}{Adjusted boiling point of water in degrees F.
    }
    \item{pres}{Atmospheric pressure, in inches of Mercury
    }
   \item{lpres}{ 100 times log10(pres), rounded to two decimals}
 }
}

\source{
Forbes, J. (1857). Further experiments and remarks on the measurement of heights and boiling point of water. \emph{Transactions of the Royal Society of Edinburgh}, 21, 235-243.}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(Forbes)
}
\keyword{datasets}
