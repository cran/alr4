\name{Heights}
\alias{Heights}
\docType{data}
\title{Pearson-Lee data}
\description{
Karl Pearson organized the collection
of data on over 1100 families in England in the period 1893 to 1898. This
particular data set gives the Heights in inches of mothers and their
daughters, with up to two daughters per mother.  All daughters are at least
age 18, and all mothers are younger than 65.  Data were given in the source
as a frequency table to the nearest inch.
Rounding error has been added to remove discreteness from graph.
}
\format{This data frame contains the following columns:
 \describe{
    \item{mheight}{
 Mother's ht, in.
    }
    \item{dheight}{
 Daughter's ht, in.
    }
 }
}

\source{
K. Pearson and  A. Lee (1903), On the laws of inheritance
in man, \emph{Biometrika}, 2, 357--463, Table 31.  
}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(Heights)
}
\keyword{datasets}
