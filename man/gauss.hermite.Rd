\name{gauss.hermite}
\title{Calculate Gauss-Hermite Quadrature Points}
\alias{gauss.hermite}
\usage{
gauss.hermite(points, iterlim=50)
}
\description{
\code{gauss.hermite} calculates the Gauss-Hermite quadrature values
for a specified number of points.  From the \pkg{rmutil} package by Jim
Lindsey (\url{https://www.commanster.eu/rcode.html})
}
\arguments{
\item{points}{The number of points.}
\item{iterlim}{Maximum number of iterations in Newton-Raphson.}
}
\value{
\code{gauss.hermite} returns a two-column matrix containing the points
and their corresponding weights.
}
\author{J.K. Lindsey}
\examples{
gauss.hermite(10)
}
\keyword{math}
