\name{UN2}
\alias{UN2}
\docType{data}
\title{ National statistics from the United Nations }
\description{
National health, welfare, and education statistics for 193
places, mostly UN members, but also other areas like Hong Kong that are not
independent countries.
}
\format{
  This data frame uses
locality name as row labels.  In some cases, the geographic area is
    smaller than a country; for example, Hong Kong.
The file contains the following columns:
  \describe{
    \item{logFertility}{log Expected number of live births per female, 2000}
    \item{logPPgdp}{Log Per capita 2001 GDP, in US \$}
    \item{Purban}{Percent of population that is urban, 2001}
  }
}
\details{
 These data were collected at published by the UN from a variety of sources.
 See original source for additional footnotes concerning values for
 individual countries.  Country names are given in the first column of the
 data file.
}
\source{
 \url{http://unstats.un.org/unsd/demographic}
}
\references{Weisberg, S. (in press). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(UN2)
}
\keyword{datasets}
