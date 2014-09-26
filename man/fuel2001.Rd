\name{fuel2001}
\alias{fuel2001}
\docType{data}
\title{Fuel consumption}
\description{
Data on motor fuel consumption and related variables, for the year 2001.
The unit is a state in the
United States or the District of Columbia.  Data are for 2001, unless noted.
}
\format{This data frame contains the following columns.  Row 
labels are the two-digit US Postal abbreviations for the US states.
 \describe{
    \item{Drivers}{
 Number of Licensed drivers in the state
    }
    \item{FuelC}{
 Gasoline sold for road use (1000s of gal.)
    }
    \item{Income}{
 Per capita personal income (year 2000)
    }
    \item{Miles}{
 Miles of Federal-aid highway miles in the state
    }
    \item{MPC}{
 Estimated miles driven per capita
    }
    \item{Pop}{
 Population age 16 and over
    }
    \item{Tax}{
 Gasoline state tax rate, cents per gallon
    }
 }
}
\source{
\url{http://www.fhwa.dot.gov/ohim/hs01/index.htm}
}

\references{
Weisberg, S. (2014). \emph{Applied Linear Regression}, third edition.  New
York:  Wiley.}

\examples{
head(fuel2001)
# Most of the examples in ALR3 that use these data first 
# transform several of the columns
fuel2001 <- transform(fuel2001,
     Dlic=1000 * Drivers/Pop,
     Fuel=1000 * FuelC/Pop,
     Income=Income/1000)
pairs(Fuel~Tax + Dlic + Income + log2(Miles), data=fuel2001)
}
\keyword{datasets}
