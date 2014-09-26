\name{Whitestar}
\alias{Whitestar}
\docType{data}
\title{Titanic Survivors}
\description{
For each person on board the fatal maiden voyage of the ocean liner
Titanic, this dataset records sex, age (adult/child), economic status
(first/second/third class, or crew) and whether or not that person
survived.  The name of the company that owned the Titanic was White Star.
Several versions of these data exist in the R universe. 
}
\format{This data frame contains the following columns:
 \describe{
    \item{surv}{
 Number of survivors
    }
    \item{m}{
 survivors + deaths
    }
    \item{class}{
 Crew or passanger class
    }
    \item{age}{
 adult or child
    }
    \item{sex}{
 male or female
    }
 }
}

\source{
Report on the Loss of the `Titanic' (S.S.) (1990), \emph{British Board of
Trade Inquiry Report} (reprint), Gloucester, UK: Allan Sutton
Publishing.  Taken from the \emph{Journal on Statistical Education Archive},
submitted by rdawson@husky1.stmarys.ca.}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(Whitestar)
}
\keyword{datasets}
