\name{Donner}
\alias{Donner}
\docType{data}
\title{Donner party}
\description{
The Donner Party was the most famous tragedy in the history of the
westward migration in the United States.  In the winter of 1846-47,
abount ninety wagon train emigrants were unable to cross the Sierra
Nevada Mountains of California before winter, and almost one-half starved
to death.  Perhaps because they were ordinary people -- farmers,
merchants, parents, children.
These data include some information about each of the members of the party
from Johnson (1996).}
\format{This data frame uses the person's name as row labels 
and contains the following columns:
 \describe{
    \item{age}{
 Approximate age in 1846
    }
    \item{y}{
 died or survived, a factor
    }
    \item{sex}{
 Male or Female
    }
    \item{family.name}{
 Either a family name, hired or single
    }
    \item{status}{
 A factor with levels Family, Single or Hired
    }
 }
}

\source{Johnson, K. (1996). \emph{Unfortunate Emigrants: Narratives of the
Donner Party.} Logan, UT: Utah State University Press, 
\url{http://www.metrogourmet.com/crossroads/KJhome.htm}.}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(Donner)
}
\keyword{datasets}
