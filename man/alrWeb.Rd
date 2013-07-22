\name{alr4Web}
\alias{alr4Web}

\title{ Access to the Applied Linear Regression website }
\description{
  These function will access the website for Applied Linear Regression,
  3rd and 4th editions.} 

\usage{
alr4Web(page = c("webpage", "errata", "primer", "solutions"))
}

\arguments{
  \item{page}{ A character string indicating what page to open. The default 
          "webpage" will open the main webpage, "errata" displays the
          Errata sheet for the thrid edition of the book, "primer" fetches and displays
          the primer for R, and "solutions" gives solutions to odd-numbered
          problems.}
}

\value{
  Either a webpage or a pdf document is displayed.  This function gives quick
access to the website for the book and in particular to the R primer and
solutions to odd-numbered problems.  The pdf files are formatted for viewing
on a computer screen.  With Adobe Reader, view the pdf files with the
bookmarks showning at the left, using signle page view which is selected
by View -> Page Dispaly -> Single Page View.
}

\author{ Sanford Weisberg, based on the function UsingR in the UsingR
package by John Verzani }


\examples{

\dontrun{alr4Web("primer")}
}
\keyword{ interface }% at least one, from doc/KEYWORDS
