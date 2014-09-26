\name{ftcollinstemp}
\alias{ftcollinstemp}
\docType{data}
\title{Ft. Collins Temperature}
\description{
Monthly average temperature data for Fort Collins, CO weather station 53005, 1900-01 to 2010-11
}
\format{This data frame contains the following columns:
 \describe{
    \item{year}{
Year corresponding to the September to November data 
    }
    \item{fall}{
September to November mean temperature, degrees F
    }
    \item{winter}{
December to February mean temperature, degrees F
    }
 }
}

\source{\url{http://ccc.atmos.colostate.edu/cgi-bin/monthlydata.pl}}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th edition. Hoboken NJ: Wiley.}
\examples{
head(ftcollinstemp)
}
\keyword{datasets}
