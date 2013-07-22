\name{landrent}
\alias{landrent}
\title{Land rent}
\description{
The data were collected by Douglas Tiffany to study the variation in rent paid in 1977 for agricultural land planted to alfalfa.
}
\format{This data frame contains the following columns:
 \describe{
    \item{X1}{
 average rent for all tillable land
    }
    \item{X2}{
 density of dairy cows (number per square mile)
    }
    \item{X3}{
 proportion of farmland used for pasture
    }
    \item{X4}{
 1 if liming required to grow alfalfa; 0 otherwise
    }
    \item{Y}{
 average rent per acre planted to alfalfa
    }
 }
}

\source{Douglas Tiffany}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(landrent)
}
\keyword{datasets}
