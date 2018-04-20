\name{UN11}
\alias{UN11}
\docType{data}
\title{
National statistics from the United Nations mostly from 2009-2011
}
\description{
National health, welfare, and education statistics for 210
places, mostly UN members, but also other areas like Hong Kong that are not
independent countries.
}
\usage{data(UN11)}
\format{
  A data frame with 237 observations on the following 32 variables.
  \describe{
    \item{\code{region}}{region of the world} 
    \item{\code{group}}{a factor with levels \code{oecd} for countries that
are members of the OECD, the Organization for Economic Co-operation and
Development, as of May 2012, \code{africa} for countries on the African
continent, and \code{other} for all other countries.  No OECD countries are
located in Africa}
    \item{\code{fertility}}{number of children per woman}
    \item{\code{ppgdp}}{Per capita gross domestic product in US dollars}
    \item{\code{lifeExpF}}{Female life expectancy, years}
    \item{\code{pctUrban}}{Percent Urban}
  }
}
\details{
Similar data, from the period 2000-2003, appears in the \code{alr3} package
under the name \code{UN3}.
}
\source{
All data were collected from UN tables accessed at \url{http://unstats.un.org/unsd/demographic/products/socind/}
on April 23, 2012.   OECD membership is from \url{www.oecd.org}, accessed
May 25, 2012..
}
\references{
Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th edition.
Hoboken NJ: Wiley.
}
\examples{
data(UN11)
## maybe str(UN11) ; plot(UN11) ...
}
\keyword{datasets}
