\name{Downer}
\alias{Downer}
\docType{data}
\title{Downer data}
\description{
For unknown reasons, some dairy cows become recumbant--they lay
down.  This condition can be serious,
and may lead to death of the cow.  These data are from a study of
blood samples of over 500 cows studied at the Ruakura (N.Z.) Animal Health
Laboratory during 1983-84.  A variety of blood tests were performed, and for
many of the animals the outcome (survived, died, or animal was killed)
was determined.  The goal is to see if survival can be predicted from the
blood measurements.
Case numbers 12607 and 11630 were noted as having exceptional care---and
they survived.
}
\format{This data frame contains the following columns:
 \describe{
    \item{calving}{
 a factor with levels before and after
    }
    \item{daysrec}{
 Days recumbent
    }
    \item{ck}{
 Serum creatine phosphokinase (U/l at 30C)
    }
    \item{ast}{
 serum asparate amino transferase (U/l at 30C)
    }
    \item{urea}{
 serum urea (mmol/l)
    }
    \item{pcv}{
 Packed Cell Volume (Haemactocrit), %
    }
    \item{inflamat}{
 inflamation 0=no, 1=yes
    }
    \item{myopathy}{
 Muscle disorder, a factor with levels present, and absent
    }
    \item{outcome}{
 a factor with levels died and survived
    }
 }
}

\source{
Clark, R. G., Henderson, H. V., Hoggard, G. K. Ellison, R. S. and Young,
B. J. (1987).  The abiltiy of biochemical and haematolgical
tests to predict recovery in periparturient recumbent cows.
\emph{NZ Veterinary Journal}, 35, 126-133.}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(Downer)
}
\keyword{datasets}
