\name{titanic}
\alias{titanic}
\docType{data}
\title{Titanic}
\description{
For each person on board the fatal maiden voyage of the ocean liner
Titanic, this dataset records sex, age (adult/child), economic status
(first/second/third class, or crew) and whether or not that person
survived. 
}
\format{This data frame contains the following columns:
 \describe{
    \item{Surv}{
 Number of survivors
    }
    \item{N}{
 Survivors + Deaths
    }
    \item{Class}{
 Crew or passanger class
    }
    \item{Age}{
 Adult or child
    }
    \item{Sex}{
 Male or female
    }
 }
}

\source{
Report on the Loss of the `Titanic' (S.S.) (1990), \emph{British Board of
Trade Inquiry Report} (reprint), Gloucester, UK: Allan Sutton
Publishing.  Taken from the \emph{Journal on Statistical Education Archive},
submitted by rdawson@husky1.stmarys.ca.}
\references{Weisberg, S. (in press). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(titanic)
}
\keyword{datasets}
