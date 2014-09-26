\name{lakes}
\alias{lakes}
\docType{data}
\title{Lake zooplankton diversity}
\description{
These data give the number of known
crustacean zooplankton species for 69 world lakes.  Also included are a number
of characteristics of each lake.  There are missing values.
}
\format{This data frame uses lake name as row label and contains the following columns:
 \describe{
    \item{Species}{
 Number of zooplankton species
    }
    \item{MaxDepth}{
 Maximum lake depth, m
    }
    \item{MeanDepth}{
 Mean lake depth, m
    }
    \item{Cond}{
 Specific conductance, micro Siemans
    }
    \item{Elev}{
 Elevation, m
    }
    \item{Lat}{
 N latitude, degrees
    }
    \item{Long}{
 W longitude, degrees
    }
    \item{Dist}{
 distance to nearest lake, km
    }
    \item{NLakes}{
 number of lakes within 20 km
    }
    \item{Photo}{
 Rate of photosynthesis, mostly by the 14C method
    }
    \item{Area}{
 Lake area, in hectares
    }
 }
}

\source{Dodson, S. (1992), Predicting curstacean zooplankton species
richness, \emph{Limnology and Oceanography}, 37, 848--856.}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(lakes)
}
\keyword{datasets}
