\name{BGSall}
\alias{BGSall}
\alias{BGSboys}
\alias{BGSgirls}
\docType{data}
\title{Berkeley guidance study}
\description{
Data from the Berkeley guidance study of children born in
 1928-29 in Berkeley, CA.
BGSall contains all the data, BGSboys the boys only, and BGSgirls the girls
only.
}
\format{This data frame contains the following columns:
 \describe{
    \item{Sex}{
 0 = males, 1 = females
    }
    \item{WT2}{
 Age 2 weight (kg)
    }
    \item{HT2}{
 Age 2 height (cm)
    }
    \item{WT9}{
 Age 9 weight (kg)
    }
    \item{HT9}{
 Age 9 height (cm)
    }
    \item{LG9}{
 Age 9 leg circumference (cm)
    }
    \item{ST9}{
 Age 9 strength (kg)
    }
    \item{WT18}{
 Age 18 weight (kg)
    }
    \item{HT18}{
 Age 18 height (cm)
    }
    \item{LG18}{
 Age 18 leg circumference (cm)
    }
    \item{ST18}{
 Age 18 strength (kg)
    }
    \item{BMI18}{
 Body Mass Index, \code{WT18/(HT18/100)^2}, rounded to one decimal.
    }
    \item{Soma}{
 Somatotype, a 1 to 7 scale of body type.
    }
 }
}
\source{
Tuddenham, R. D. and Snyder, M. M. (1954).
Physical Growth of California Boys and Girls from Birth to
Eighteen years. Univ. of Calif.
Publications in Child Development, 1, 183-364.}
\references{
S. Weisberg (2014).  \emph{Applied Linear Regression}, 4th edition.  
Hoboken NJ: Wiley.
}
\examples{
head(BGSall)
head(BGSboys)
head(BGSgirls)
}
\keyword{datasets}
