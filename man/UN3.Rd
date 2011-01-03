\name{UN3}
\alias{UN3}
\docType{data}
\title{ National statistics from the United Nations }
\description{
National health, welfare, and education statistics for 210
places, mostly UN members, but also other areas like Hong Kong that are not
independent countries.  Only 125 localities have data present for all the variables.
}
\format{
  This data frame uses locality name as row labels.
It contains the following columns:
  \describe{
    \item{ModernC}{Percent of unmarried women using a modern method of
    contraception.}
    \item{Change}{Annual population growth rate, percent.}
    \item{PPgdp}{Per capita 2001 GDP, in US \$.}
    \item{workF}{Percent of females over age 15 economically active.}
    \item{Pop}{Population, thousands.}
    \item{Fertility}{Expected number of live births per female, 2000}
    \item{Purban}{Percent of population that is urban, 2001}
}}
\details{
 These data were collected 2000-2003 and published by the UN from a variety of sources.
 See original source for additional footnotes concerning values for
 individual countries.  Missing values in the data file indicated values
 for which no data is available, and will generally occur in less-developed 
 localities.  
}
\source{
More recent values for these variables can be obtained from the following 
web pages:
(1) \code{ModernC}: 
\url{http://unstats.un.org/unsd/demographic/products/socind/contraceptive.htm};
(2) \code{change}: 
\url{http://unstats.un.org/unsd/demographic/products/socind/population.htm};
(3) \code{PPgdp}:
\url{http://unstats.un.org/unsd/demographic/products/socind/inc-eco.htm};
(4) \code{Frate}:  
\url{http://unstats.un.org/unsd/demographic/products/socind/inc-eco.htm};
(5) \code{Pop}:
\url{http://unstats.un.org/unsd/demographic/products/socind/population.htm};
(6) \code{Fertility}:
\url{http://unstats.un.org/unsd/demographic/products/indwm/tab2c.htm};
(7) \code{Purban}:
\url{http://unstats.un.org/unsd/demographic/products/socind/hum-sets.htm}.
}
\references{Weisberg, S. (in press). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.
}
\examples{
head(UN3)
}
\keyword{datasets}
