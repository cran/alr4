\name{Aovtest}
\alias{Aovtest}
\docType{data}
\title{
Artificial unbalanced anova data.
}
\description{
Artificial data with three factors and a covariate.  Used as a homework problem
in Weisberg (in press) to explore the various types of analysis of variance.
}
\usage{data(Aovtest)}
\format{
  A data frame with 120 observations on the following 5 variables.
  \describe{
    \item{\code{a}}{a factor with levels \code{1} \code{2}}
    \item{\code{b}}{a factor with levels \code{1} \code{2} \code{3}}
    \item{\code{c}}{a factor with levels \code{1} \code{2} \code{3} \code{4}}
    \item{\code{x}}{a continuous covariate}
    \item{\code{y}}{the response}
    \item{\code{good}}{logical with values \code{FALSE} for observations to
    be deleted to get an unbalanced data set and \code{TRUE} otherwise}
  }
}
\details{
The data were generated from a 2 by 3 by 4 design with five observations per cell
and one covariate.  Use \code{good} to select about 80\% of the data to set up
an unbalanced data set.
}
\references{
Weisberg, S. (in press). \emph{Applied Linear Regression}, 4rd edition.
Hoboken: Wiley.
}
\examples{
m1 <- lm(y ~ x + (a + b + c)^3, Aovtest) # all data
m2 <- update(m1, subset=good)  # unbalanced version
m3 <- update(m1, ~ .- x) # orthogonal version, ignore the covariate
}
\keyword{datasets}
