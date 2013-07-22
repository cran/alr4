\name{ftcollinssnow}
\alias{ftcollinssnow}
\docType{data}
\title{Ft. Collins snowfall}
\description{
Monthly snowfall data for Fort Collins, CO, 1900-01 to 1992-93
}
\format{This data frame contains the following columns:
 \describe{
    \item{YR1}{
Year corresponding to the September to December data 
    }
    \item{Early}{
September to December snowfall, inches
    }
    \item{Late}{
January to June snowfall, inches
    }
 }
}

\source{\url{http://ccc.atmos.colostate.edu/cgi-bin/monthlydata.pl}}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(ftcollinssnow)
}
\keyword{datasets}
