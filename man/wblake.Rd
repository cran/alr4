\name{wblake}
\alias{wblake}
\docType{data}
\title{West Bearskin Lake small mouth bass data.}
\description{
Data on samples of small mouth bass collected in West Bearskin Lake, Minnesota,
in 1991.  The file \code{wblake} includes only fish of ages 8 or younger.
}
\format{This data frame contains the following columns:
 \describe{
    \item{Age}{
 Age at capture (yrs)
    }
    \item{Length}{
 Length at capture (mm)
    }
    \item{Scale}{
 radius of a key scale, mm
    }
 }
}

\source{Minnesta Department of Natural Resources}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th edition. Hoboken NJ: Wiley.}
\examples{
head(wblake)  # excludes fish age 9 or older
}
\keyword{datasets}
