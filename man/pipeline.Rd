\name{pipeline}
\alias{pipeline}
\title{Alaska pipeline}
\description{
The Alaska pipeline data consists of in-field ultrasonic
measurements of the depths of defects in the Alaska pipeline. The
depth of the defects were then re-measured in the laboratory.
These measurements were performed in six different batches. The
data were analyzed to calibrate the bias of the field measurements
relative to the laboratory measurements. In this analysis, the
field measurement is the response variable and the laboratory
measurement is the predictor variable.

These data were originally provided by Harry Berger, who was at the time a
scientist for the Office of the Director of the Institute of
Materials Research (now the Materials Science and Engineering
Laboratory) of NIST. These data were used for a study conducted
for the Materials Transportation Bureau of the U.S. Department of
Transportation.
}
\format{This data frame contains the following columns:
 \describe{
    \item{Field}{
Number of defects measured in the field. 
    }
    \item{Lab}{
Number of defects measured in the field. 
    }
    \item{Batch}{
Batch number 
    }
 }
}

\source{
\url{http://www.itl.nist.gov/div898/handbook/pmd/section6/pmd621.htm}
}
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(pipeline)
}
\keyword{datasets}
