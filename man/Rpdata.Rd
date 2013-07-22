\name{Rpdata}
\alias{Rpdata}
\docType{data}
\title{
Always plot residuals.
}
\description{
This example with aritifical data is designed to demonstrate the importance of
plotting residuals.
}
\usage{data(Rpdata)}
\format{
  A data frame with 990 observations on the following 7 variables.
  \describe{
    \item{\code{y}}{a numeric vector}
    \item{\code{x1}}{a numeric vector}
    \item{\code{x2}}{a numeric vector}
    \item{\code{x3}}{a numeric vector}
    \item{\code{x4}}{a numeric vector}
    \item{\code{x5}}{a numeric vector}
    \item{\code{x6}}{a numeric vector}
  }
}

\source{
Data generated using programs from
\url{http://www4.stat.ncsu.edu/~stefanski/NSF_Supported/Hidden_Images/stat_res_plots.html}
}
\references{
Stefanski, L. A. (2007).  Residual (sur)Realism.  The American Statistician,
61, 163-177. url={https://www.amstat.org/about/pdfs/NCSUStatsProfSurpriseHomework.pdf}.
}
\examples{
data(Rpdata)
\dontrun{
require(car)
residualPlot(lm(Rpdata))
}
}
\keyword{datasets}
