\name{wblake}
\alias{wblake}
\alias{wblake2}
\docType{data}
\title{West Bearskin Lake small mouth bass data.}
\description{
Data on samples of small mouth bass collected in West Bearskin Lake, Minnesota,
in 1991.  The file \code{wblake} includes only fish of ages 8 or younger,  
while \code{wblake2} adds a few older fish.
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
\references{Weisberg, S. (in press). \emph{Applied Linear Regression}, 4th edition. Hoboken NJ: Wiley.}
\examples{
head(wblake)  # excludes fish age 9 or older
head(wblake2)  # all fish
}
\keyword{datasets}
