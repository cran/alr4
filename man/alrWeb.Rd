\name{alrWeb}
\alias{alrWeb}
\alias{alr4Web}

\title{ Access to the Applied Linear Regression website }
\description{
  These function will access the website for Applied Linear Regression,
  3rd and 4th editions.} 

\usage{
alrWeb(page = c("webpage", "errata", "primer"), script)

alr4Web(page = c("webpage", "errata", "primer", "facebook"), script)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{page}{ A character string indicating what page to open. The default 
          "webpage" will open the main webpage, "errata" displays the
          Errata sheet for the thrid edition of the book, "primer" fetches and displays
          the primer for R, and "facebook" goes to the facebook page for
          the draft of the fourth edition.}
  \item{script}{If set, this will open an R script for one of the chapters
          in the book in a browser window, or a script from one of the
          chapters in the R Primer.  Possible values are "chapter1",
          "chapter2", ... "chapter12", "appendix" for chapters and
          appendix from the book, or
          "primer0", "primer1", ... "primer12", "primerappendix"
          for material corresponding to the R primer.  Not yet implemented
          for \code{alr4Web}.} 
}

\value{
  Either a webpage or a pdf document is displayed.  This function gives quick
access to the website for the book and in particular to the R primer.

\code{alrWeb} is relevant to the 3ed edition of the textbook, while \code{alr4Web}
gets documents for the 4th edition.  The names of these functions will change
wehn the 4th edition is published.
}

\author{ Sanford Weisberg, based on the function UsingR in the UsingR
package by John Verzani }


\examples{

\dontrun{alr4Web("primer")}
}
\keyword{ interface }% at least one, from doc/KEYWORDS
