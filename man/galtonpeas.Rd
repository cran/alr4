\name{galtonpeas}
\alias{galtonpeas}
\docType{data}
\title{Galton's peas}
\description{
In a paper presented to the Royal Institute on February 9, 1877, Sir Francis Galton discussed his experiments on sweet peas in which he compared the sweet peas produced by parent plants to those produced by offspring plants. In these experiments he could observe inheritance from one generation to the next. Galton categorized the parent plants according to the typical diameter of the peas they produced.
}
\format{This data frame contains the following columns:
 \describe{
    \item{Parent}{
 mean diameter of parent
    }
    \item{Progeny}{
 mean diameter of offspring
    }
    \item{SD}{
 offspring standard deviation
    }
 }
}

\source{Pearson, K. (1930). \emph{Life and Letters and Labours of Francis Galton}, Vol IIIa. Cambridge: Cambridge University Press.}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(galtonpeas)
}
\keyword{datasets}
