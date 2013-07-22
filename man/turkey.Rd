\name{turkey}
\alias{turkey}
\docType{data}
\title{Turkey data, all sources}
\description{
Data from an experiment on the growth of turkeys.  60 pens of turkeys were
grown with a similar diet, supplemented with a dose of methionine from one of
three sources.  The response is average pen weight.  Recorded is dose, source,
m, always 5 except for dose=0, average weight gain, and within group SS.
}
\format{This data frame contains the following columns:
 \describe{
    \item{A}{
 Dose: Amount of supplement as a percent of the total diet
    }
    \item{Gain}{
 Ave. weight gain, over all replications
    }
    \item{S}{
    A factor for the source of methionine, three levels numbers 1, 2 and 3.
    }
    \item{m}{
 Number of replications or pens
    }
    \item{SD}{
 SD of the m pens with the same values of S and A.
    }
 }
}

\source{
R. D. Cook and J. Witmer (1985). A note on parameter-effects curvature.
\emph{Journal of the American Statistical Association}, 80, 872--878.}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(turkey)
}
\keyword{datasets}
