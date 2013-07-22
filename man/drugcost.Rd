\name{drugcost}
\alias{drugcost}
\docType{data}
\title{Drug cost.}
\description{
These data are to try to understand the effect of health plan characteristics
on drug costs.  Health plans vary in size, given as member months.  Some plans
use generic drugs more than others.  All differ on copayments.  Some have
strong restrictions on which drugs can be dispensed
value of RI=0 means that all drugs are dispensed, RI=100 means that only
one per category is avaiable.  The goal is to determine the terms that are
related to cost, and in particular to understand the role of GS and RI in
determining cost.
}
\format{This data frame uses a short code name for the drug plan as row
labels and contains the following columns:
 \describe{
    \item{COST}{
 Ave. cost to plan for 1 prescription for 1 day
    }
    \item{RXPM}{
 Number of prescriptions per member per year
    }
    \item{GS}{
 Percent generic substitution, number between 0 (no substitution) to 100 (always use generic substitute)
    }
    \item{RI}{
 Restrictiveness index (0=none, 100=total)
    }
    \item{COPAY}{
 Average Rx copayment
    }
    \item{AGE}{
 Average age of member
    }
    \item{F}{
 Percent female members
    }
    \item{MM}{
 Member months, a measure of the size of the plan
    }
 }
}

\source{Mark Siracuse}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(drugcost)
}
\keyword{datasets}
