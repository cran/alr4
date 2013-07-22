\name{Stevens}
\alias{Stevens}
\docType{data}
\title{
Subjective scaling data collected by S. S. Stevens
}
\description{
This experiment was apparently done by S. S. Stevens and colleagues in March 1962,
although the exact reference is lost.  10 subjects were played tones at each of
5 loudnesses, presumably in random order.  Subjects were asked to
draw a line on paper whose length matched the loudness of the tone.  Each
subject repeated each loudness 3 times, for a total of 30 trials per subject.
The original data are lost; reported here is the mean of the 3 log-lengths for
each loudness, the sd of the three log-lengths, and the number of replications,
which is always 3.
}
\usage{data(Stevens)}
\format{
  A data frame with 50 observations on the following 5 variables.
  \describe{
    \item{\code{subject}}{a factor with unique values for each subject}
    \item{\code{loudness}}{either 50, 60, 70, 80 or 90 db.
Decibels are a logrithmic scale}
    \item{\code{y}}{a numeric vector giving the mean of the log-lengths of
three lines drawn.  Exponentiating these values would give the geometric mean
of the three lengths in cm.}
    \item{\code{sd}}{a numeric vector, giving the sd of the three log lengths}
    \item{\code{n}}{a numeric vector, equal to the constant value 3}
  }
}
\details{
This is a classic example of a psychophysics experiment pioneered by S. S.
Stevens.  The basic idea is that the phychological response y to a physical
stimulus x should be proportional to x to a power.  Since both the response
and the loudness are already in log-scale, linear fits should be expected.
}
\source{
These data were obtained in the early 1970s from the data library in the
Harvard University Statistics Department.
}
\references{
Stevens, S. S. (1966).  A metric for social consensus, \emph{Science}, 151,
530-541, \url{http://www.jstor.org/stable/1717034} 
}
\examples{
head(Stevens)
}
\keyword{datasets}
