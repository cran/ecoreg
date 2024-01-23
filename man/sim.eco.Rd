\name{sim.eco}
\alias{sim.eco}
\title{Simulate ecological data and samples of individual-level data}
\description{
Simulate ecological data and samples of individual-level data from an 
individual-level logistic regression model, depending on given binary,
categorical or normally-distributed covariates. 
}
\usage{
sim.eco(N, ctx, binary, m, data=NULL, S=0, cross=NULL, covnames, ncats,
mu, alpha.c=0, alpha=0, beta=0, sig=0, strata, pstrata, isam = 0)
}
\arguments{
  \item{N}{ Vector of population sizes, one for each group. }

  \item{ctx}{ A model formula containing names of group-level, or
    contextual, covariates on the right-hand side. }

  \item{binary}{ A model formula containing names of individual-level
    binary covariates on the right-hand side. }

  \item{data}{ Data frame containing the group-level variables given in
    \code{ctx}.  It should also contain the variables given in \code{binary}, interpreted as
    proportions of individuals exposed to each of the binary covariates. }   
  
  \item{m}{ A data frame with \code{length(N)} rows, containing the
    within-area means of a set of normally-distributed continuous
    covariates.}

  \item{S}{ A data frame with \code{length(N)} rows, containing the
    within-area standard deviations of a set of normally-distributed
    continuous covariates.  For the moment they are assumed to be
    independent.}

  \item{cross}{ A matrix of cross-classifications of individuals in the
    area between categories of multiple binary or categorical covariates, defined in
    the same way as in \code{\link{eco}}.   If this is not supplied, the
    binary covariates are assumed to be independent, and the probability
    of an individual having a certain combination of covariates is
    calculated as the product of the relevant marginal probabilities. }

  \item{covnames}{ Vector of names of the covariates, if \code{cross}
    is supplied.  Otherwise the names are taken from \code{binary}. } 

  \item{ncats}{ Numeric vector of the number of levels of the
    covariates used in \code{cross}. } 

  \item{mu}{ Regression intercept on the logit scale. }

  \item{alpha.c}{ Vector of coefficients for the group-level
    covariates in the underlying logistic regression, corresponding to
    the columns of \code{ctx}. }

  \item{alpha}{ Vector of coefficients for the individual-level
    binary covariates, corresponding to the columns of
    \code{phi}. Interactions are not currently supported. }

  \item{beta}{ Vector of coefficients for the individual-level
    continuous covariates, corresponding to the columns of \code{m} or
    \code{S}. Interactions are not currently supported. }

  \item{sig}{ Random-effects standard deviation. }

  \item{strata}{ A matrix with rows representing groups, and columns
    representing strata occupancy probabilities.  } 

  \item{pstrata}{ A vector with one element for each stratum, giving
    the assumed baseline outcome probabilities for the strata. The
    logits of \code{pstrata} are used as offsets in the logistic regression. } 

  \item{isam}{ Number of individuals per group to retain in the
    individual-level data. }
}
\value{
  A list with components: 
  \item{y}{The simulated aggregate-level response, one for each group.}
  \item{idata}{ A data frame containing the retained individual-level
    samples. The grouping indicator (with values \code{1} to \code{length(N)})
    is named \code{group}, the response variable is named \code{y}, 
    the group-level covariates are copied from \code{ctx}, and the
    binary covariates, with values 1 or 0, are individual-level
    equivalents of the proportions given in \code{phi}.  }
}
\author{ C. H. Jackson \email{chris.jackson@mrc-bsu.cam.ac.uk} }
\seealso{ \code{\link{eco}} }
\examples{
N <- rep(50, 20)
ctx <- cbind(deprivation = rnorm(20), mean.income = rnorm(20))
phi <- cbind(nonwhite = runif(20), smoke = runif(20))
sim.df <- as.data.frame(cbind(ctx, phi))
mu <- qlogis(0.05)  ## Disease with approximate 5\% prevalence
## Odds ratios for group-level deprivation and mean imcome
alpha.c <- c(1.01, 1.02)
## Odds ratios for individual-level ethnicity and smoking
alpha <- c(1.5, 2) 
sim.eco(N, ctx = ~ deprivation + mean.income, binary = ~ nonwhite +
        smoke, data=sim.df, mu=mu, alpha.c=alpha.c, alpha=alpha)
sim.eco(N, ctx = ~ deprivation + mean.income, binary = ~ nonwhite +
        smoke, data=sim.df, mu=mu, alpha.c=alpha.c, alpha=alpha, isam=3)
}
\keyword{ datagen }
