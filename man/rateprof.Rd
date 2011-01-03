\name{Rateprof}
\alias{Rateprof}
\docType{data}
\title{
Professor ratings from RateMyProfessors.com.
}
\description{
These data includes the summaries of the ratings of 364 instructors at one 
large campus in the Midwest from Bleske-Rechek and Fritsch (2011).  Each 
instructor included in the data had at least 10 ratings over a several year 
period.  Students provided ratings on 5 point scales.  The data file provides 
the averages ratings and additional characteristics of the instructors
}
\format{
  A data frame with 364 observations on the following 17 variables.
\describe{
    \item{\code{gender}}{instructor gender, a factor with levels \code{female} \code{male}}
    \item{\code{numYears}}{a numeric vector, number of years in which this
instructor had ratings between 1999 and 2009.  }
    \item{\code{numRaters}}{number of ratings}
    \item{\code{numCourses}}{number of different course titles included in
the rating for this instructor}
    \item{\code{pepper}}{a factor with levels \code{no} and \code{yes}.  In addition
to rating for quality, instructors are rated as attractive or not.   A value of \code{yes} means that the consensus is that
the instructor is attractive.  }
    \item{\code{discipline}}{a factor with levels \code{Hum} for humanities, 
\code{SocSci} for social sciences,
\code{STEM} for science, technology, engineering and mathematics and  
\code{Pre-prof} for professional training}
    \item{\code{dept}}{a factor with department names \code{Accounting},
\code{Anthropology}, \code{Art}, \code{Art and design}, \code{Art History}, 
\code{Astronomy/Physics}, \code{Biology}, \code{Business}, \code{Chemistry}, 
\code{Communication}, \code{Communication Disorders}, \code{Computer Science}, 
\code{Criminal Justice}, \code{Curriculum and Instruction}, \code{Dance}, 
\code{Economics}, \code{English}, \code{Environmental Public Health}, 
\code{Finance}, \code{FLTR}, \code{French}, \code{Geography}, \code{Geology}, 
\code{German}, \code{History}, \code{Information Systems}, \code{Japanese}, 
\code{Kins}, \code{Library Science}, \code{Management}, \code{Managerial Science}, 
\code{Marketing}, \code{Math}, \code{Music}, \code{Nursing}, \code{Philosophy}, 
\code{Physics}, \code{Physics & Astronomy}, \code{Physics and Astronomy}, 
\code{Political Science}, \code{Psychology}, \code{Religious Studies}, 
\code{Social Work}, \code{Sociology}, \code{Spanish}, \code{Special Education}, 
\code{Theater}, \code{Womens Studies},}
    \item{\code{quality}}{Average quality rating, between 1, worst, to 5, best}
    \item{\code{helpfulness}}{Average helfpulness rating, between 1, worst, 
to 5, best}
    \item{\code{clarity}}{Average clarity rating, between 1, worst, to 5, best}
    \item{\code{easiness}}{Average easiness rating, between 1, worst, to 5, best}
    \item{\code{raterInterest}}{Average rater interest, between 1, lowest, 
to 5, highest}
    \item{\code{sdQuality}}{SD of quality rating}
    \item{\code{sdHelpfulness}}{SD of helpfulness rating}
    \item{\code{sdClarity}}{SD of clarity rating}
    \item{\code{sdEasiness}}{SD of easiness rating}
    \item{\code{sdRaterInterest}}{SD of rater interest}
  }
}

\source{
Provided by April Bleske-Rechek.
}
\references{
Bleske-Rechek, A. and Fritsch, A. (2011). Student Consensus on RateMyProfessors.com.
Practical Assessment, Research \& Evaluation, 16(18), 
\url{http://pareonline.net/getvn.asp?v=16&n=18}
}
\examples{
data(Rateprof)
}
\keyword{datasets}
