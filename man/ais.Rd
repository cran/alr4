\name{ais}
\alias{ais}
\docType{data}
\title{Australian institute of sport data}
\description{
Data on 102 male and 100 female athletes collected at the Australian
Institute of Sport.
}
\format{This data frame contains the following columns:
 \describe{
    \item{Sex}{
 (0 = male or 1 = female)
    }
    \item{Ht}{
 height (cm)
    }
    \item{Wt}{
 weight (kg)
    }
    \item{LBM}{
 lean body mass
    }
    \item{RCC}{
 red cell count
    }
    \item{WCC}{
 white cell count
    }
    \item{Hc}{
 Hematocrit
    }
    \item{Hg}{
 Hemoglobin
    }
    \item{Ferr}{
 plasma ferritin concentration
    }
    \item{BMI}{
 body mass index, weight/(height)**2
    }
    \item{SSF}{
 sum of skin folds
    }
    \item{Bfat}{
 Percent body fat
    }
    \item{Label}{
 Case Labels
    }
    \item{Sport}{
 Sport
    }
 }
}

\source{Ross Cunningham and Richard Telford}
\references{
S. Weisberg (2014).  \emph{Applied Linear Regression}, 4th edition.  New York:
Wiley.}

\examples{
head(ais)
}
\keyword{datasets}
