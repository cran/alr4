\name{highway}
\alias{highway}
\docType{data}
\title{Highway accidents}
\description{
The data comes from a unpublished master's paper by Carl Hoffstedt. They relate the automobile accident rate, in accidents per million vehicle miles to several potential terms.  The data include 39 sections of large highways in the state of Minnesota in 1973.  The goal of this analysis was to understand the impact of design variables, Acpts, Slim, Sig, and Shld that are under the control of the highway department, on accidents.
}
\format{This data frame contains the following columns:
 \describe{
    \item{ADT}{
 average daily traffic count in thousands
    }
    \item{Trks}{
 truck volume as a percent of the total volume
    }
    \item{Lane}{
 total number of lanes of traffic
    }
    \item{Acpt}{
 number of access points per mile
    }
    \item{Sigs}{
 number of signalized interchanges per mile
    }
    \item{Itg}{
 number of freeway-type interchanges per mile
    }
    \item{Slim}{
 speed limit in 1973
    }
    \item{Len}{
 length of the highway segment in miles
    }
    \item{Lwid}{
 lane width, in feet
    }
    \item{Shld}{
 width in feet of outer shoulder on the roadway
    }
    \item{Hwy}{An indicator of the type of roadway or the source of funding
    for the road;
 0 if MC, 1 if FAI, 2 if PA, 3 if MA
    }
    \item{Rate}{
 1973 accident rate per million vehicle miles
    }
 }
}

\source{Carl Hoffstedt}
\references{Weisberg, S. (in press). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(highway)
}
\keyword{datasets}
