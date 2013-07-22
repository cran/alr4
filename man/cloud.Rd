\name{cloud}
\alias{cloud}
\docType{data}
\title{Florida area cumulus experiment, FACE I.}
\description{
The data summarize the results of the first Florida Area Cumulus Experiment, or FACE-1, designed to study the effectiveness of cloud seeding to increase rainfall in a target area (Woodley, Simpson, Biondini, and Berkley, 1977).
}
\format{This data frame contains the following columns:
 \describe{
    \item{A}{
 Action, 1=seed, 0=do not seed
    }
    \item{D}{
 Day after June 16, 1975
    }
    \item{S}{
 Suitability for seeding
    }
    \item{C}{
 percent cloud cover in experimental area, measured using radar in Coral Gables, Florida
    }
    \item{P}{
 \eqn{10^7 m^3} prewetness
    }
    \item{E}{
 echo motion category, either 1 or 2, a measure for type of cloud
    }
    \item{Rain}{
 \eqn{10^7 m^3} in target area
    }
 }
}

\source{Woodley, W.L., Simpson, J., Biondini, R., and Berkley, J. (1977). Rainfall results 1970-75: Florida area cumulus experiment. \emph{Science}, 195, 735-742.}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(cloud)
}
\keyword{datasets}
