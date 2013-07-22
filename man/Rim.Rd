\name{MinnLand}
\alias{MinnLand}
\docType{data}
\title{
Conservation restrictions and farm prices in Minnesota
}
\description{
These data include nearly every farm sale in 6 economic regions in Minnesota 
from 2002-2011 that either has
land enrolled in the federal Conservation Reserve Program, or CRP, or has
no restrictions.  A few sales with non-crp land easements were excluded.  
CRP enrollment is for a fixed period during which farmers
agree not to grow crops for a fixed payment.  This can effect sale price of
land since buyers have fewer choices on use of land that could lower values,
but also have guaranteed income for a fixed period that could raise values.
}
\usage{data(MinnLand)}
\format{
  A data frame with 18700 observations on the following 10 variables.
  \describe{  
    \item{\code{acrePrice}}{sale price in dollars per acre. Sale prices were
adjusted to a common date within the year.  No inflation adjustment is made
between years. 
    }
    \item{\code{region}}{a factor with levels giving the geographic names of
       six economic regions of Minnesota.  Excluded economic regions had
few farm sales.}
    \item{\code{improvements}}{percentage of property value 
due to improvements. Minnesota assessors estimate values separately for land and buildings. 
This variable is the ratio of the building value to 
the total value.}
    \item{\code{year}}{year of sale, as a continuous variable, not as a factor.  Most
uses of this variable would require converting it to a factor.}
    \item{\code{acres}}{size of the farm in acres}
    \item{\code{tillable}}{percentage of farm acreaage that is rated 
     arable by the assessor }
    \item{\code{financing}}{a factor with levels \code{title transfer} and
    \code{seller finance}}
    \item{\code{crpPct}}{the percentage of all farm acres enrolled in CRP}
    \item{\code{productivity}}{average agronomic productivity 
scaled 1 to 100, with larger numbers for more productive land. 
This score is based on University of Minnesota soil studies.  This value is
frequently missing because some counties never had the study done, and some
county assessors are inconsistent in including this value in the record of
the sale.
}
}}

\details{
Data is collected from Minnesota counties.  Some counties do not include the 
\code{productivity} value in sales records, accounting for
most of the missing values.  The variable \code{tillable} is also frequently
missing.  
}

\source{
S. J. Taff
}

\references{
Taff, S. J. and Weisberg, S. (2007).  Compensated shrot-term conservation 
 restrictions may reduce sale prices.  \emph{The Appraisal Journal}, 75(1), 45. 
}
\examples{
head(MinnLand)
\dontrun{
require(mice)
md.pattern(MinnLand)
}
}
\keyword{datasets}
