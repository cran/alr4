\name{Highway}
\alias{Highway}
\docType{data}
\title{Highway accidents}
\description{
The data comes from a unpublished master's paper by Carl Hoffstedt. They relate the automobile accident rate, in accidents per million vehicle miles to several potential terms.  The data include 39 sections of large Highways in the state of Minnesota in 1973.  The goal of this analysis was to understand the impact of design variables, acpts, slim, Sig, and shld that are under the control of the Highway department, on accidents.
}
\format{This data frame contains the following columns:
 \describe{
    \item{adt}{
 average daily traffic count in thousands
    }
    \item{trks}{
 truck volume as a percent of the total volume
    }
    \item{lane}{
 total number of lanes of traffic
    }
    \item{acpt}{
 number of access points per mile
    }
    \item{sigs}{
 number of signalized interchanges per mile
    }
    \item{itg}{
 number of freeway-type interchanges per mile
    }
    \item{slim}{
 speed limit in 1973
    }
    \item{len}{
 length of the Highway segment in miles
    }
    \item{lwid}{
 lane width, in feet
    }
    \item{shld}{
 width in feet of outer shoulder on the roadway
    }
    \item{htype}{An indicator of the type of roadway or the source of funding
    for the road;   "mc" for major collector, "fai" for Federal interstate highways,
    "pa" for principal arterial highway, and "ma" for major arterial highways
    }
    \item{rate}{
 1973 accident rate per million vehicle miles
    }
 }
}

\source{Carl Hoffstedt}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(Highway)
}
\keyword{datasets}
