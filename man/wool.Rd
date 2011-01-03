\name{wool}
\alias{wool}
\docType{data}
\title{Wool data}
\description{
This is a three-factor experiment with each factor at three levels, for
a total of 27 runs.  Samples of worsted yarn were with different levels of
the three factors were given a cyclic load until the sample failed.  The
goal is to understand how cycles to failure depends on the factors.
}
\format{This data frame contains the following columns:
 \describe{
    \item{Len}{
 length of specimen (250, 300, 350 mm)
    }
    \item{Amp}{
 amplitude of loading cycle (8, 9, 10 min)
    }
    \item{Load}{
 load (40, 45, 50g)
    }
    \item{Cycles}{
 number of cycles until failure
    }
 }
}

\source{
Box, G. E. P. and Cox, D. R. (1964).  An analysis of transformations (with
discussion).  \emph{J. Royal Statist. Soc.}, B26, 211-46.}
\references{Weisberg, S. (in press). \emph{Applied Linear Regression}, 4th edition. Hoboken NJ: Wiley.}
\examples{
head(wool)
}
\keyword{datasets}
