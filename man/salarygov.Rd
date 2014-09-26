\name{salarygov}
\alias{salarygov}
\docType{data}
\title{Government salary study}
\description{
Data on non-unionized job classes in a US county in 1986.  Included are the
job class difficulty score, the number of employees in the class, number of
female employees, and the name of the class.
}
\format{This data frame contains the following columns:
 \describe{
    \item{JobClass}{
 Name of job class
    }
    \item{NW}{
 Number of women employees
    }
    \item{NE}{
 Total number of employees in a job class
    }
    \item{Score}{
 Difficulty score for job class
    }
    \item{MaxSalary}{
 Maximum salary for job class
    }
 }
}

\source{Sanford Weisberg}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(salarygov)
}
\keyword{datasets}
