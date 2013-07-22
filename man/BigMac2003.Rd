\name{BigMac2003}
\alias{BigMac2003}
\docType{data}
\title{World cities data}
\description{
Prices in many world cities from a
2003 Union Bank of Switzerland report.}

\format{This data frame uses the name of the city as row names, and
contains the following columns:
 \describe{
    \item{BigMac}{
 Minutes of labor to purchase a Big Mac 
    }
    \item{Bread}{
 Minutes of labor to purchase 1 kg of bread
    }
    \item{Rice}{
 Minutes of labor to purchase 1 kg of rice
    }
    \item{FoodIndex}{
 Food price index (Zurich=100)
    }
    \item{Bus}{
 Cost in US dollars for a one-way 10 km ticket
    }
    \item{Apt}{
 Normal rent (US dollars) of a 3 room apartment
    }
    \item{TeachGI}{
 Primary teacher's gross income, 1000s of US dollars
    }
    \item{TeachNI}{
 Primary teacher's net income, 1000s of US dollars
    }
    \item{TaxRate}{
 Tax rate paid by a primary teacher
    }
    \item{TeachHours}{
 Primary teacher's hours of work per week:
    }
 }
}

\source{Union Bank of Switzerland report, \emph{Prices and Earnings Around
the Globe} (2003 version), from \url{http://www.ubs.com/1/e/ubs_ch/wealth_mgmt_ch/research.html}.}
\references{Weisberg, S. (2014).  \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(BigMac2003)
}
\keyword{datasets}
