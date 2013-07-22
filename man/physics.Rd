\name{physics}
\alias{physics}
\alias{physics1}
\docType{data}
\title{Physics data}
\description{
The file physics constains results for \eqn{\pi^+}{pi+} meson as input and
\eqn{\pi^+}{pi+} meson as
output.  physics1 is for \eqn{\pi^-}{pi-} to \eqn{\pi^-}{pi-}.
}
\format{This data frame contains the following columns:
 \describe{
    \item{x}{
 Inverse total energy
    }
    \item{y}{
 Scattering cross-section/sec
    }
    \item{SD}{
 Standard deviation
    }
 }
}

\source{
Weisberg, H., Beier, H., Brody, H., Patton, R., Raychaudhari, K., Takeda, H.,
Thern, R. and Van Berg, R. (1978).
s-dependence of proton fragmentation by hadrons. II.  Incident laboratory
momenta, 30--250 GeV/c.  \emph{Physics Review D}, 17, 2875--2887.  }
\references{Weisberg, S. (2014). \emph{Applied Linear Regression}, 4th
edition. Hoboken NJ: Wiley.}
\examples{
head(physics1)
}
\keyword{datasets}
