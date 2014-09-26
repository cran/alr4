\name{florida}
\alias{florida}
\docType{data}
\title{Florida presidential election}
\description{
County-by-county vote for president in Florida in 2000 for Bush, Gore and Buchanan.
}
\format{
  A data frame three vaiaables for each of Florida's 67 counties.
  \describe{
    \item{Gore}{Vote for Gore}
    \item{Bush}{Vote for Bush}
    \item{Buchanan}{Vote for Buchanan}
  }
}
\source{
\url{http://www.abcnews.go.com.sections/politics/2000vote/general/FL_county.html}.
This web address does not seem to exist any longer.
}
\references{
Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th edition.
Hoboken NJ: Wiley.
}
\examples{
head(florida)
## maybe str(florida) ; plot(florida) ...
}
\keyword{datasets}
