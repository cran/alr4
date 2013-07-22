\name{salary}
\alias{salary}
\docType{data}
\title{Salary data}
\description{
Salary of faculty in a small Midwestern college in the early 1980s.  
}
\format{This data frame contains the following columns:
 \describe{
    \item{degree}{
 Factor with levels "PhD" or "Masters"
    }
    \item{rank}{
 Factor, "Asst", "Assoc" or "Prof"
    }
    \item{sex}{
 Factor, "Male" or "Female"
    }
    \item{year}{
 Years in current rank
    }
    \item{ysdeg}{
 Years since highest degree earned
    }
    \item{salary}{
 dollars per year
    }
 }
}

\source{Sanford Weisberg}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4rd
edition. Hoboken NJ: Wiley.}
\examples{
head(salary)
}
\keyword{datasets}
