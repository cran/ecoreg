\name{eco}
\alias{eco}
\title{Ecological regression using aggregate and/or individual data}
\description{
  Estimation of an underlying individual-level logistic regression
  model, using aggregate data alone, individual-level data alone or a
  combination of aggregate and individual-level data. Any number
  number of covariates can be included in the individual-level
  regression. Covariates can be binary or categorical, expressed as proportions
  over the group, or normally-distributed, expressed as
  within-area means and optional covariances.  A general formula for
  group-level (contextual) effects can also be supplied.
}
\usage{
eco(formula, binary, categorical, normal, iformula, data, idata, groups, igroups,
strata, istrata, pstrata, cross=NULL, norm.var=NULL, random=FALSE,
pars, fixed=FALSE, model = c("marginal","conditional"),
outcome=c("binomial","poisson"), gh.points=10, iter.adapt=5, ...)
}
\arguments{
  \item{formula}{ A model formula containing the group-level binomial
    response on the left-hand side, and general group-level covariates on the
    right-hand side.  For example,

    \code{cbind(n.cases, population) ~ mean.income + deprivation.index}

    If \code{formula} is not specified, then there is assumed to be only
    individual-level data, and \code{iformula} should be supplied. 
  }

  \item{binary}{ An optional model formula with an empty left-hand
side. The right-hand side should contain the names of any group-level
    proportions, which are to modelled as individual-level binary
    predictors of the response given in \code{formula}.  For example,

    \code{~ p.smokers + p.nonwhite + p.unemployed}
  }

  \item{categorical}{ An optional list of matrices or data frames.
    Each element corresponds to a categorical covariate.  Each element
    has the same number of rows as the aggregate data, and number of columns
    corresponding to the number of levels of the categorical covariate.
    The cells give the number or proportion of individuals in the area in each
    category.  These will be modelled as individual-level predictors of
    the response given in \code{formula}.
  }
  
  \item{normal}{ An optional model formula with an empty left-hand
    side. The right-hand side should list variables containing the
    group-level means of normally-distributed covariates.  These will be
    modelled as individual-level predictors of the response given in
    \code{formula}. For example

    \code{~ pollution + income}.
  }

  \item{iformula}{ A model for the corresponding individual-level data. The
    individual-level binary response should be on the right-hand side,
    and the individual-level covariates should be on the left-hand side.
    They should represent the same covariates, in the same order, as given in
    \code{formula} and \code{binary} respectively. However they need not
    have the same names.  For example

    \code{outcome ~ mean.income + deprivation.index + smoking + nonwhite + unemployed}.

    If \code{iformula} is not specified, then there is assumed to be
    only aggregate data, and \code{formula} should be supplied. 

  }
  
  \item{data}{ Data frame containing the group-level variables given in
    \code{formula} and \code{binary}. }

  \item{idata}{ Data frame containing the individual-level variables
    given in \code{iformula}. }

  \item{groups}{ A group-level variable containing the group identifiers
    to be matched with the groups given in \code{igroups}. Defaults to
    the row numbers of the aggregate data.  Only necessary if the model
    includes random group effects. }

  \item{igroups}{ An individual-level variable containing the group
    identifiers of the individual-level data to be matched with the
    groups given in \code{groups}. Only necessary if the model
    includes random group effects. }

  \item{strata}{ A matrix with the same number of rows as the aggregate
  data. Rows representing groups, and columns
    representing strata occupancy probabilities, often estimated as 
    observed occupancy proportions.  The relative risks for the strata
    will be included as fixed offsets in the underlying logistic
    regression, using the probabilites supplied in \code{pstrata}.  This
    is to save the computational burden of estimating the "nuisance"
    strata-specific risks from the data. }

  \item{istrata}{ A variable containing the individual-level variable
    indicating the stratum an individual occupies. This should be a
    factor with the levels corresponding to the columns of the matrix
    \code{strata}.  }

  \item{pstrata}{ A vector with one element for each stratum, giving
    the assumed baseline outcome probabilities for the strata.  }

  \item{cross}{ A matrix giving the joint within-area distribution of
    all the covariates supplied in \code{binary} and \code{categorical}
    and any strata.  This should have the same number of rows as the
    aggregate data, and number of columns equal to the product of the
    numbers of levels of the covariates and strata, for example
    \eqn{2^n} if there are \eqn{n} binary covariates.  Each
    cell gives the proportion of individuals in the area occupying a 
    category defined by a unique combination of the covariates.   The
    categories are given in the order

    column 1: covariate 1 absent, covariate 2 absent, ..., covariate n-1 absent, covariate n absent \cr
    column 2: covariate 1 present, covariate 2 absent, ..., covariate n-1 absent, covariate n absent \cr
    column 3: covariate 1 absent, covariate 2 present, ..., covariate n-1 absent, covariate n absent  \cr
    column 4: covariate 1 present, covariate 2 present, ..., covariate n-1 absent, covariate n absent  \cr
    etc.

    (assuming \eqn{n} binary covariates, with the obvious generalisation
    for categorical covariates) If \code{strata} are used, these are taken as
    covariate n+1.
  } 

  \item{norm.var}{ A data frame, matrix or list, supplying the within-area covariances of the continuous covariates.    
    
    If \code{norm.var} is a data frame or matrix, then the continuous
    covariates are assumed to be independent within areas.  It should
    have rows corresponding to areas, columns corresponding to continuous
    covariates, each cell giving the within-area standard deviation of
    the covariate.

    If \code{norm.var} is a list, then it should have the same number of
    elements as the number of areas, and each element should be the
    within-area covariance matrix of the continuous covariates.

    \code{norm.var} can also be the name of a variable in \code{data}
    which contains the standard deviation of a single continuous
    covariate. 
  } 

  \item{random}{ If \code{TRUE} then a normally-distributed random
    group-level intercept, with zero mean, is also included in the model. }

  \item{pars}{ Vector of initial values of the model parameters, given
    in the following order:

    logit-scale intercept,\cr
    coefficients for group-level covariates,\cr
    coefficients for individual-level covariates,\cr
    random effects standard deviation.\cr

    If not supplied, the initial values are 0 for all covariate effects,
    1 for the random effects standard deviation.  The intercept is
    initialised to the logit mean outcome proportion over groups from
    the aggregate data. }

  \item{fixed}{ If \code{TRUE} then \code{\link{eco}} just calculates the
    likelihood with all parameters are fixed at their
    initial values. }

  \item{model}{ If "marginal" then the ecological group-level risk is based on
    integrating over binary individual-level covariates.  This is suitable if the
    aggregate exposures are estimated using a survey of individuals in the area. 
    If  "conditional" then the binary individual-level covariates are
    conditioned on, and the group-level risk is the normal approximation
    model described by Wakefield (2004).  This is suitable if the
    aggregate exposures are estimated using a full population census. }

  \item{outcome}{ Distribution of the aggregate outcome, by default
    "binomial". \code{outcome="poisson"} can be specified for rare
    outcomes. }
  
  \item{gh.points}{ Number of points for Gauss-Hermite numerical
    integration in the random effects model. }
  
  \item{iter.adapt}{ Number of adaptive iterations to estimate the mode and scale
    for Gauss-Hermite numerical integration in the random-effects
    model. }
  
  \item{\dots}{ Arguments passed to \code{\link{optim}}. }
}
\details{
  Individual data are simply modelled by a logistic regression.
  
  Aggregate outcomes are modelled as binomial, with area-level risk
  obtained by integrating the underlying individual-level logistic
  regression model over the within-area distribution of the covariates.

  The model for combined individual and aggregate data shares the same
  coefficients between the individual and aggregate components.

  Aggregate data alone can be sufficient for inference of
  individual-level relationships, provided the between-area variability
  of the exposures is large compared to the within-area variability. 
  
  When there are several binary covariates, it is usually advisable to
  account for their within-area distribution, using \code{cross}.

  See Jackson et al. (2006,2008) for further details. 
}
\value{
  A list with components:

  \item{call}{The call to \code{\link{eco}}.}

  \item{lik}{Minus twice the log-likelihood at the estimates.}

  \item{ors.ctx}{Matrix of estimated odds ratios and 95\% confidence
    intervals for the area-level covariates.}

  \item{ors.indiv}{Matrix of estimated odds ratios and 95\% confidence
    intervals for the individual-level covariates.}

  \item{random}{The estimated random-effects standard deviation.}

  \item{mod}{A list of constants describing the model and data (not useful
    to end users).}
  
  \item{corrmat}{The correlation matrix of the maximum likelihood
    estimates (on the optimized scale, for example log odds ratios for
    covariates).}
  
}
\references{

  C. H. Jackson, N. G. Best, and S. Richardson. (2006) \emph{Improving
  ecological inference using individual-level data.} Statistics in
  Medicine, 25(12): 2136-2159.

  C. H. Jackson, N. G. Best, and S. Richardson. (2008)
  \emph{Hierarchical related regression for combining aggregate and
    survey data in studies of socio-economic disease risk factors.}
  Journal of the Royal Statistical Society, Series A, 171(1):159-178.
  
  J. Wakefield. (2004) \emph{Ecological inference for 2 x 2 tables}
  (with discussion). Journal of the Royal Statistical Society, Series A,
  167(3) 385--445.

  J. Wakefield and R. Salway. (2001) \emph{A statistical framework for
  ecological and aggregate studies}.  Journal of The Royal Statistical
  Society, Series A, 164(1):119--137, 2001.
}
\author{C. H. Jackson \email{chris.jackson@mrc-bsu.cam.ac.uk}}
\seealso{ \code{\link{sim.eco}} }
\examples{

## Simulate some aggregate data and some combined aggregate and
## individual data. 
ng <- 50
N <- rep(100, ng)
set.seed(1)
ctx <- cbind(deprivation = rnorm(ng), mean.income = rnorm(ng))
phi <- cbind(nonwhite = runif(ng), smoke = runif(ng))
sim.df <- as.data.frame(cbind(ctx, phi))
mu <- qlogis(0.05)  ## Disease with approximate 5\% prevalence
alpha.c <- log(c(1.01, 1.02))  ## Odds ratios for group-level deprivation and mean imcome
alpha <- log(c(1.5, 2)) ## Odds ratios for individual-level ethnicity and smoking
sim1 <- sim.eco(N, ctx=~deprivation+mean.income, binary=~nonwhite+smoke, data = sim.df,  mu=mu, alpha.c=alpha.c, alpha=alpha)
sim2 <- sim.eco(N, ctx=~deprivation+mean.income, binary=~nonwhite+smoke, data = sim.df,  mu=mu, alpha.c=alpha.c, alpha=alpha, isam=7)

## Fit the model to recover the simulated odds ratios.
aggdata <- as.data.frame(cbind(y=sim1$y, sim.df))
agg.eco <- eco(cbind(y, N) ~ deprivation + mean.income,
               binary = ~ nonwhite + smoke,  data = aggdata)
agg.eco

## Combining with individual-level data
## doesn't improve the precision of the estimates.
agg.indiv.eco <- eco(cbind(y, N) ~ deprivation + mean.income,
               binary = ~ nonwhite + smoke,
               iformula = y ~ deprivation + mean.income + nonwhite + smoke, 
               data = aggdata, idata=sim2$idata)
agg.indiv.eco

## However, suppose we have much lower between-area variance in the
## mean covariate value.
phi <- cbind(nonwhite = runif(ng, 0, 0.3), smoke = runif(ng, 0.1, 0.4))
sim.df <- as.data.frame(cbind(ctx, phi))
sim1 <- sim.eco(N, ctx=~deprivation+mean.income, binary=~nonwhite+smoke, data = sim.df,  mu=mu, alpha.c=alpha.c, alpha=alpha)
sim2 <- sim.eco(N, ctx=~deprivation+mean.income, binary=~nonwhite+smoke, data = sim.df,  mu=mu, alpha.c=alpha.c, alpha=alpha, isam=10)
aggdata <- as.data.frame(cbind(y=sim1$y, sim.df))

## The aggregate data now contain little information about the
## individual-level effects, and we get biased estimates of the true individual model. 
agg.eco <- eco(cbind(y, N) ~ deprivation + mean.income,
               binary = ~ nonwhite + smoke,  data = aggdata)
agg.eco

## We need individual-level data to be able to estimate the
## individual-level effects accurately. 
agg.indiv.eco <- eco(cbind(y, N) ~ deprivation + mean.income,
               binary = ~ nonwhite + smoke,
               iformula = y ~ deprivation + mean.income + nonwhite + smoke, 
               data = aggdata, idata=sim2$idata)
agg.indiv.eco

## But then why not just study the individual data?  Combining with
## aggregate data improves precision.  
indiv.eco <- eco(iformula = y ~ deprivation + mean.income + nonwhite + smoke, 
               idata=sim2$idata)
indiv.eco

}
\keyword{models}
\keyword{regression}
\keyword{nonlinear}
