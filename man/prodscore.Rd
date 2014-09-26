\name{prodscore}
\alias{prodscore}
\docType{data}
\title{Soil productivity}
\description{
Soil productivity scores for farms in townships in four counties in the 
Minneapolis St. Paul metropolitan area, 1981-82.  The goal is to see if 
the productivity score is a good predictor of the assessed value of the 
farmland.  If so, then productivity score could be used to set assesed value
for farms enrolled in the ``green acres'' program that requires that urban
farmland be taxed at its agricultural value only without regard to
development potential.
}
\format{This data frame contains the following columns:
 \describe{
    \item{County}{
 Name of the county
    }
    \item{Value}{
Assessed value in dollars per acre.
    }
    \item{P}{
Productivity score, a number between 1 and 100. 
    }
    \item{Year}{
Tax year, either 1981 or 1982. 
    }
 }
}

\source{Douglas Tiffany}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(prodscore)
}
\keyword{datasets}
