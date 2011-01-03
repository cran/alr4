\name{banknote}
\alias{banknote}
\title{Swiss banknote data}
\description{
Six measurements made on 100 genuine
Swiss banknotes and 100 counterfeit ones.
}
\format{This data frame contains the following columns:
 \describe{
    \item{Length}{
 Length of the banknote near the top, mm
    }
    \item{Left}{
 Height of the left edge of the banknote, mm
    }
    \item{Right}{
 Height of the right edge of the banknote, mm
    }
    \item{Bottom}{
 Distance from bottom of banknote to beginning of patterned border, mm
    }
    \item{Top}{
 Distance from bottom of banknote to beginning of patterned border, mm
    }
    \item{Diagonal}{
 Length of image diagonal, mm
    }
    \item{Y}{
 0 = genuine, 1 = counterfeit
    }
 }
}

\source{
Flury, B. and Riedwyl, H. (1988). \emph{Multivariate Statistics: A practical approach.} London: Chapman & Hall.}
\references{Weisberg, S. (in press).  \emph{Applied Linear Regression}, 4rd
edition. Hoboken NJ: Wiley.}
\examples{
head(banknote)
}
\keyword{datasets}
