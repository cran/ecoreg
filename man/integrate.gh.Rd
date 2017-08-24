\name{integrate.gh}
\alias{integrate.gh}
\title{Univariate Gauss-Hermite integration}
\description{
  Computes the integral of a univariate function, or several univariate
  functions simultaneously, using Gauss-Hermite quadrature. 
}
\usage{
integrate.gh(h, n=1, points = 10, mu = 0, scale = 1, ...)
}
\arguments{
  \item{h}{ The function to be integrated. May either have a scalar
    first argument and return a scalar result, or have a first argument of
    length \code{n} and return a vector of \code{n} results, corresponding to \code{n}
    independent functions.  }

  \item{n}{ The dimension of the result returned by \code{h}. }

  \item{points}{ Number of Gauss-Hermite quadrature points.  }

  \item{mu}{ Mode of the function, to centre the quadrature points
    around. }

  \item{scale}{ Scale of the quadrature points. }

  \item{\dots}{ Other arguments to be passed to \code{h}. }
}
\value{
  The integral of \eqn{h(x)} between \code{-Inf} and \code{Inf}, of length
  \code{n}.  In the usual application of Gauss-Hermite quadrature,
  \eqn{h(x)} is equivalent to a function \eqn{g(x)\phi(x)}{g(x)phi(x)}, where
  \eqn{\phi(x)}{phi(x)} is the standard normal density function. 
}
\details{
  The integral is more accurate if the standard quadrature points are
  shifted and scaled to match the mode and scale of \eqn{g(x)}, that is
  the objective function divided by the standard normal density.  The
  scale is estimated by
  \eqn{1/\sqrt{-H}}{1 /sqrt(-H)}, where H is the Hessian at the maximum
  of \eqn{g(x)}.

}
\references{
  Liu, Q. and Pierce, D. A. (1994) \emph{A note on Gauss-Hermite
    quadrature}. Biometrika, 81 (624-629)
}
\author{C. H. Jackson \email{chris.jackson@mrc-bsu.cam.ac.uk}

  The Gauss-Hermite polynomial values and weights are calculated using
  the \code{\link{gauss.hermite}} function copied from the \pkg{rmutil}
  package by J. K. Lindsey. 
}
\seealso{
  \code{\link{gauss.hermite}}
}
\examples{
## Want the integral of h over the real line
g <- function(x) 4 * exp( - ((1 - x)^2 + 1))
h <- function(x) g(x) * dnorm(x)
integrate(h, -Inf, Inf)
integrate.gh(h)
## Not very accurate with default 10 points. Either use more quadrature points, 
integrate.gh(h, points=30)
## or shift and scale the points.
opt <- nlm(function(x) -g(x), 0, hessian=TRUE)
integrate.gh(h, mu=opt$estimate, scale=1/sqrt(opt$hessian))

}
\keyword{math}
