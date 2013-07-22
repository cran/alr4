\name{cakes}
\alias{cakes}
\docType{data}
\title{Cakes data}
\description{
Oehlert (2000, Example 19.3) provides data from a small experiment on baking packaged cake mixes.
}
\format{
  A data frame with 14 observations on the following 4 variables.
  \describe{
    \item{block}{a factor}
    \item{X1}{Baking time, minutes}
    \item{X2}{Baking temperature, degrees F}
    \item{Y}{Palatability score}
  }
}

\source{
 Oehlert, G. W. (2000).  \emph{A First Course in Design and Analysis of Experiments}. New York:  Freeman.
}
\references{
Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th edition.
Hoboken NJ: Wiley.
}
\examples{
head(cakes)
lm(Y~X1+X2+I(X1^2)+I(X2^2)+X1:X2, data=cakes)

}
\keyword{datasets}
