\name{blowdown}
\alias{blowdown}
\alias{blowAPB}
\alias{blowBF}
\title{Blowdown data}
\description{
Data from the Boundary Waters Canoe Area Wilderness Blowdown.  The data frame blowdown
includes nine species of trees,
blowAPB gives data for aspen and
paper birch, while 
blowBF gives data for Balsam Fir only.
}
\format{This data frame contains the following columns:
 \describe{
    \item{D}{
Tree diameter, in cm
    }
    \item{S}{
 Proportion of basal area killed for the four species BF, C, PB, BS, a measure of
 local severity of the storm.
    }
    \item{y}{
 1 of the tree died, 0 if it survived
    }
    \item{SPP}{
BF= balsam fir, BS= black spruce, C= cedar, JP= jackpine, PB= paper birch,
RP= red pine, RM= red maple, BA = black ash, A= aspen.
    }
 }
}
\source{Roy Rich}
\examples{
head(blowBF)
}
\references{
S. Weisberg (in press).  \emph{Applied Linear Regression}, third edition.  New York:  Wiley.
}
\keyword{datasets}
