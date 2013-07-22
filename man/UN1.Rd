\name{UN1}
\alias{UN1}
\docType{data}
\title{ National statistics from the United Nations }
\description{
Demographic data for 193
places, mostly UN members, but also other areas like Hong Kong that are not
independent countries.
}
\format{
  This data frame uses the locality name as a row label.  In some cases the
geographic area is smaller than a country; for example Hong Kong.  The file
contains the following columns:
  \describe{
    \item{Fertility}{Expected number of live births per female, 2000}
    \item{PPgdp}{Per capita 2001 GDP, in US \$}
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
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(UN1)
}
\keyword{datasets}
