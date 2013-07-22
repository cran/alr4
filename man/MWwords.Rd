\name{MWwords}
\alias{MWwords}
\docType{data}
\title{Word frequencies from Mosteller and Wallace}
\description{
The data give the frequencies of words in works from four different sources: the political writings of eighteenth century American political figures Alexander Hamilton, James Madison, and John Jay, and the book \emph{Ulysses} by twentieth century Irish writer James Joyce.}

\format{This data frame uses the word as row labels and contains the following columns:
 \describe{
    \item{Hamilton}{
Hamilton frequency 
    }
    \item{HamiltonRank}{
Hamilton rank 
    }
    \item{Madison}{
Madison frequency 
    }
    \item{MadisonRank}{
Madison rank 
    }
    \item{Jay}{
Jay frequency 
    }
    \item{JayRank}{
Jay rank 
    }
    \item{Ulysses}{
Word frequency in \emph{Ulysses}
    }
    \item{UlyssesRank}{
Word rank in \emph{Ulysses}
    }
 }
}

\source{Mosteller, F. and Wallace, D. (1964). \emph{Inference and Disputed Authorship: The Federalist.} Reading, MA: Addison-Wesley.}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(MWwords)
}
\keyword{datasets}
