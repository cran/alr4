\name{ufc}
\alias{ufc}
\alias{ufcgf}
\alias{ufcwc}
\alias{ufcdf}
\docType{data}
\title{Height-diameter data for Upper Flat Creek, Idaho}
\description{
These data are forest inventory measures from the Upper Flat Creek stand of
the University of Idaho Experimental Forest, dated 1991.  

The file \code{ufc} contains all the data.  
\code{ufcwc} contains only Western red cedar.
\code{ufcgf} contains only grand fir.
}
\format{
  A data frame with the following 5 variables.
  \describe{
    \item{Plot}{Plot number}
    \item{Tree}{Tree within plot}
    \item{Species}{a factor with levels 
\code{DF} = Douglas-fir,
\code{GF} = Grand fir,
\code{SF} = Subalpine fir,
\code{WL} = Western larch,
\code{WC} = Western red cedar,
\code{WP} = White pine
}
    \item{Dbh}{Diameter 137 cm perpendicular to the bole, mm }
    \item{Height}{Height of the tree, in decimeters}
  }
}
\source{
Andrew Robinson
}
\references{
Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th  edition.  New
York:  Wiley.}
\examples{
head(ufcgf)
}
\keyword{datasets}
