\name{downer}
\alias{downer}
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
    \item{Calving}{
 0 if measured before calving, 1 if after
    }
    \item{Daysrec}{
 Days recumbent
    }
    \item{CK}{
 Serum creatine phosphokinase (U/l at 30C)
    }
    \item{AST}{
 serum asparate amino transferase (U/l at 30C)
    }
    \item{Urea}{
 serum urea (mmol/l)
    }
    \item{PCV}{
 Packed Cell Volume (Haemactocrit), %
    }
    \item{Inflamat}{
 inflamation 0=no, 1=yes
    }
    \item{Myopathy}{
 Muscle disorder, 1 if present, 0 if absent
    }
    \item{Outcome}{
 outcome:  1 if survived, 0 if died or killed
    }
 }
}

\source{
Clark, R. G., Henderson, H. V., Hoggard, G. K. Ellison, R. S. and Young,
B. J. (1987).  The abiltiy of biochemical and haematolgical
tests to predict recovery in periparturient recumbent cows.
\emph{NZ Veterinary Journal}, 35, 126-133.}
\references{Weisberg, S. (in press). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(downer)
}
\keyword{datasets}
