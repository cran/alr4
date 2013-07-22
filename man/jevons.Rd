\name{jevons}
\alias{jevons}
\docType{data}
\title{Jevon's gold coin data}
\description{
In a study of coinage, W. Stanley Jevons weighed 274 gold sovereigns that he had collected from circulation in Manchester, England. For each coin, he recorded the weight, after cleaning, to the nearest .001 gram, and the date of issue.  The age classes are coded 1 to 5, roughly corresponding to the age of the coin in decades.  The standard weight of a gold sovereign was suppose to be 7.9876 grams; minimum legal weight was 7.9379 grams.
}
\format{This data frame contains the following columns:
 \describe{
    \item{Age}{
Age of coins, decades 
    }
    \item{n}{
Number of coins 
    }
    \item{Weight}{
Average weight, grams 
    }
    \item{SD}{
Standard deviation. 
    }
    \item{Min}{
Minimum weight 
    }
    \item{Max}{
Maximum weight
    }
 }
}

\source{Stephen Stigler}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(jevons)
}
\keyword{datasets}
