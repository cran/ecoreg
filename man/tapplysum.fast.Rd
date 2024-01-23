\name{tapplysum.fast}
\alias{tapplysum.fast}
\title{Simplified fast group sums}
\description{
  A hack to speed up tapply(x, group, sum) for the special case where
  x is sorted by group. 
}
\usage{
tapplysum.fast(x, groups)
}
\arguments{
  \item{x}{A numeric vector.}
  \item{groups}{A grouping factor.}
}
\value{
  Vector containing the group sums of \code{x}. 
}
\details{
  Works by computing the cumulative sum of x and taking the difference
  at the indices where the groups change.  Standard \code{\link{tapply}} can
  be slow when there are a large number of groups, due to the overhead
  of factor manipulation. 
}
\seealso{
  \code{\link{tapply}}
}
\examples{
x <- factor(rep(1:1000, each=100))
y <- rnorm(1000*100)
system.time(tapply(y, x, sum))
system.time(tapplysum.fast(y, x))
}
\author{C. H. Jackson <chris.jackson@mrc-bsu.cam.ac.uk>}
\keyword{manip}
