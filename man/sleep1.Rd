\name{sleep1}
\alias{sleep1}
\docType{data}
\title{Sleep in mammals}
\description{
Includes species averages for 62 mammals. 
}
\format{This data frame uses spcies as row lable and contains the following columns:
 \describe{
    \item{SWS}{
 Slow wave nondreaming sleep, hrs/day
    }
    \item{PS}{
 Paradoxical dreaming sleep, hrs/day
    }
    \item{TS}{
 Total sleep, hrs/day
    }
    \item{BodyWt}{
 Body weight in kg
    }
    \item{BrainWt}{
 Brain weight in g
    }
    \item{Life}{
 Maximum life span, years
    }
    \item{GP}{
 Gestation time, days
    }
    \item{P}{
 Predation index, 1=low,5=hi
    }
    \item{SE}{
 Sleep exposure index 1=exposed, 5=protected
    }
    \item{D}{
 Danger index, 1=least, 5=most
    }
 }
}

\source{
Allison, T. and Cicchetti, D. (1976). 
Sleep in
Mammals: Ecological and Constitutional Correlates 
\emph{Science}, vol. 194, pp. 732-734.}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(sleep1)
}
\keyword{datasets}
