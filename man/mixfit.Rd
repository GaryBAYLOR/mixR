% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{mixfit}
\alias{mixfit}
\title{Finite Mixture Modeling for Raw Data and Binned Data}
\usage{
mixfit(
  x,
  ncomp = NULL,
  family = c("normal", "weibull", "gamma", "lnorm"),
  pi = NULL,
  mu = NULL,
  sd = NULL,
  ev = FALSE,
  mstep.method = c("bisection", "newton"),
  init.method = c("kmeans", "hclust"),
  tol = 1e-06,
  max_iter = 500
)
}
\arguments{
\item{x}{a numeric vector for the raw data or a three-column matrix for the binned data}

\item{ncomp}{a positive integer specifying the number of components of the mixture model}

\item{family}{a character string specifying the family of the mixture model. It can only be
one element from \code{normal}, \code{weibull}, \code{gamma} or \code{lnorm}.}

\item{pi}{a vector of the initial value for the proportion}

\item{mu}{a vector of the initial value for the mean}

\item{sd}{a vector of the initial value for the standard deviation}

\item{ev}{a logical value controlling whether each component has the same variance when
fitting normal mixture models. It is ignored when fitting other mixture models. The default is \code{FALSE}.}

\item{mstep.method}{a character string specifying the method used in M-step of the EM algorithm
when fitting weibull or gamma mixture models. It can be either \code{bisection} or \code{newton}.
The default is \code{bisection}.}

\item{init.method}{a character string specifying the method used for providing initial values
for the parameters for EM algorithm. It can be one of \code{kmeans} or \code{hclust}. The default is
\code{kmeans}}

\item{tol}{the tolerance for the stopping rule of EM algorithm. It is the value to stop EM algorithm when the two
consecutive iterations produces loglikelihood with difference less than \code{tol}. The default value is 1e-6.}

\item{max_iter}{the maximum number of iterations for the EM algorithm (default 500).}
}
\value{
the function \code{mixfit} return an object of class \code{mixfitEM}, which contains a list of
different number of items when fitting different mixture models. The common items include
\item{pi}{a numeric vector representing the estimated proportion of each component}
\item{mu}{a numeric vector representing the estimated mean of each component}
\item{sd}{a numeric vector representing the estimated standard deviation of each component}
\item{iter}{a positive integer recording the number of EM iteration performed}
\item{loglik}{the loglikelihood of the estimated mixture model for the data \code{x}}
\item{aic}{the value of AIC of the estimated model for the data \code{x}}
\item{bic}{the value of BIC of the estimated model for the data \code{x}}
\item{data}{the data \code{x}}
\item{comp.prob}{the probability that \code{x} belongs to each component}
\item{family}{the family the mixture model belongs to}
For the Weibull mixture model, the following extra items are returned.
\item{k}{a numeric vector representing the estimated shape parameter of each component}
\item{lambda}{a numeric vector representing the estimated scale parameter of each component}
For the Gamma mixture model, the following extra items are returned.
\item{alpha}{a numeric vector representing the estimated shape parameter of each component}
\item{lambda}{a numeric vector representing the estimated rate parameter of each component}
For the lognormal mixture model, the following extra items are returned.
\item{mulog}{a numeric vector representing the estimated logarithm mean of each component}
\item{sdlog}{a numeric vector representing the estimated logarithm standard deviation of
each component}
}
\description{
This function is used to perform the maximum likelihood estimation for
a variety of finite mixture models for both raw and binned data by using
the EM algorithm, together with Newton-Raphson algorithm or bisection method when necessary.
}
\details{
The function \code{mixfit} is the core function in this package. It is used to perform
the maximum likelihood estimation for finite mixture models from the families of normal,
weibull, gamma or lognormal by using the EM algorithm. When the family is \code{weibull}
or \code{gamma}, the M-step of the EM algorithm has no closed-form solution and we can
use Newton algorithm by specifying \code{method = "newton"} or use bisection method by
specifying \code{method = "bisection"}.

The initial values of the EM algorithm can be provided by specifying the proportion of each
component \code{pi}, the mean of each component \code{mu} and the standard deviation of
each component \code{sd}. If one or more of these initial values are not provided, then
their values are estimated by using K-means clustering method or hierarchical clustering
method. If all of \code{pi}, \code{mu}, and \code{sd}
are not provided, then \code{ncomp} should be provided so initial values are automatically
generated. For the normal mixture models, we can
control whether each component has the same variance or not.
}
\examples{
## fitting the normal mixture models
set.seed(103)
x <- rmixnormal(200, c(0.3, 0.7), c(2, 5), c(1, 1))
data <- bin(x, seq(-1, 8, 0.25))
fit1 <- mixfit(x, ncomp = 2)  # raw data
fit2 <- mixfit(data, ncomp = 2)  # binned data
fit3 <- mixfit(x, pi = c(0.5, 0.5), mu = c(1, 4), sd = c(1, 1))  # providing the initial values
fit4 <- mixfit(x, ncomp = 2, ev = TRUE)  # setting the same variance

## (not run) fitting the weibull mixture models
## x <- rmixweibull(200, c(0.3, 0.7), c(2, 5), c(1, 1))
## data <- bin(x, seq(0, 8, 0.25))
## fit5 <- mixfit(x, ncomp = 2, family = "weibull")  # raw data
## fit6 <- mixfit(data, ncomp = 2, family = "weibull")  # binned data

## (not run) fitting the Gamma mixture models
## x <- rmixgamma(200, c(0.3, 0.7), c(2, 5), c(1, 1))
## data <- bin(x, seq(0, 8, 0.25))
## fit7 <- mixfit(x, ncomp = 2, family = "gamma")  # raw data
## fit8 <- mixfit(data, ncomp = 2, family = "gamma")  # binned data

## (not run) fitting the lognormal mixture models
## x <- rmixlnorm(200, c(0.3, 0.7), c(2, 5), c(1, 1))
## data <- bin(x, seq(0, 8, 0.25))
## fit9 <- mixfit(x, ncomp = 2, family = "lnorm")  # raw data
## fit10 <- mixfit(data, ncomp = 2, family = "lnorm")  # binned data

}
\seealso{
\code{\link{plot.mixfitEM}}, \code{\link{density.mixfitEM}},
\code{\link{select}}, \code{\link{bs.test}}
}
