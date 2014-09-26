\name{rat}
\alias{rat}
\docType{data}
\title{Rat data}
\description{
Data collected in an experiment in which rats were injected with a dose of a
drug approximately proportional to body weight.  At the end of the experiment,
the animal's liver was weighed, and the fraction of the drug recoved in the
liver was recorded.  The experimenter expected the response to be independent
of the predictors.
}
\format{This data frame contains the following columns:
 \describe{
    \item{BodyWt}{
 BodyWt of the rat
    }
    \item{LiverWt}{
 LiverWt measured after sacrifice
    }
    \item{Dose}{
 Dose, roughly proportional to body weight
    }
    \item{y}{
 dose of drug recovered after sacrifice of the animal
    }
 }
}

\source{Dennis Cook}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(rat)
}
\keyword{datasets}
