\name{UN11}
\alias{UN11}
\alias{UN11a}
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
    \item{\code{pop}}{Population, in thousands}
    \item{\code{medAge}}{Median age in the population}
    \item{\code{fracF}}{Fraction of populatino that is female}
    \item{\code{popGrowthRate}}{Population growth rate, percent}
    \item{\code{pctUrban}}{Percent Urban}
    \item{\code{lifeExpF}}{Female life expectancy, years}
    \item{\code{lifeExpM}}{Male life expectancy, years}
    \item{\code{teenFertility}}{births per 1000 teenage women}
    \item{\code{fertility}}{a numeric vector}
    \item{\code{modernContraception}}{Percentage of women using modern contraception}
    \item{\code{adultLiteracy}}{Adult literacy rate, percent}
    \item{\code{literacyM}}{Male adult literacy rate, percent}
    \item{\code{literacyF}}{Female adult literacy rate, percent}
    \item{\code{primaryEnrollF}}{Percent of females enrolled in elementary education}
    \item{\code{primaryEnrollM}}{Percent of males enrolled in elementary education}
    \item{\code{secondaryEnrollF}}{Percent of females enrolled in secondary education}
    \item{\code{secondaryEnrollM}}{Percent of males enrolled in secondary education}
    \item{\code{yrsSchool}}{Average years of school attendance}
    \item{\code{yrsSchoolM}}{Male average years of school attendance}
    \item{\code{yrsSchoolF}}{Female average years of school attendance}
    \item{\code{ppgdp}}{Per capita gross domestic product in US dollars}
  }
}
\details{
Similar data, from the period 2000-2003, appears in the \code{alr3} package
under the name \code{UN3}.
}
\source{
All data were collected from UN tables accessed at \url{http://unstats.un.org/unsd/demographic/products/socind/}
on April 23, 2012.   OECD membership is from \url{www.oecd.org}, accessed
May 25, 2012.
The data file \code{UN11a} contains only the columns \code{region},
\code{subregion}, \code{Fertility},
\code{PPgdp} and \code{lifeExpF} for localities with all these variables observed.
}
\references{
Weisberg, S. (in press). \emph{Applied Linear Regression}, 4rd edition.
Hoboken NJ: Wiley.
}
\examples{
data(UN11)
## maybe str(UN11) ; plot(UN11) ...
}
\keyword{datasets}
