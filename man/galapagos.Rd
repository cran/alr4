\name{galapagos}
\alias{galapagos}
\docType{data}
\title{Galapagos species data}
\description{
Johnson and Raven (1973) have presented data giving the number of species and related variables for 29 different islands in the Galapagos Archipelago.
}
\format{This data frame uses the island name as row labels 
and contains the following columns:
 \describe{
    \item{NS}{
 Number of Species
    }
    \item{ES}{
 Number of endemic species (orrur only on that island)
    }
    \item{Area}{
 Surface area of island, hectares
    }
    \item{Anear}{
 Area of closest island, hectares
    }
    \item{Dist}{
 Distance to closest island, km
    }
    \item{DistSC}{
 Distance from Santa Cruz Island, km
    }
    \item{Elevation}{
 Elevation in m, missing values given as zero
    }
    \item{EM}{
 1 if elevation is observed, 0 if missing
    }
 }
}

\source{Johnson, M.P., and Raven, P.H. (1973). Species number and endemism: The Galapagos Archipelago revisited. \emph{Science}, 179, 893-895.}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(galapagos)
}
\keyword{datasets}
