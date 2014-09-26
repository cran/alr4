\name{sniffer}
\alias{sniffer}
\docType{data}
\title{Sniffer data}
\description{
When gasoline is pumped into a tank, hydrocarbon vapors are forced out and into the atmosphere.  To reduce this significant source of air pollution, devices are installed to capture the vapor.  In testing these vapor recovery systems, a "sniffer" measures the amount recovered. John Rice provided the data for the file sniffer.txt.
}
\format{This data frame contains the following columns:
 \describe{
    \item{TankTemp}{
 Initial tank temperature (degrees F)
    }
    \item{GasTemp}{
 Temperature of the dispensed gasoline (degrees F)
    }
    \item{TankPres}{
 Initial vapor pressure in the tank (psi)
    }
    \item{GasPres}{
 Vapor pressure of the dispensed gasoline (psi)
    }
    \item{Y}{
 Hydrocarbons emitted (grams)
    }
 }
}

\source{John Rice}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(sniffer)
}
\keyword{datasets}
