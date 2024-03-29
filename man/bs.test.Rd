% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bs.test.R
\name{bs.test}
\alias{bs.test}
\title{Bootstrap Likelihood Ratio Test for Finite Mixture Models}
\usage{
bs.test(
  x,
  ncomp = c(1, 2),
  family = c("normal", "weibull", "gamma", "lnorm"),
  B = 100,
  ev = FALSE,
  mstep.method = c("bisection", "newton"),
  init.method = c("kmeans", "hclust"),
  tol = 1e-06,
  max_iter = 500
)
}
\arguments{
\item{x}{a numeric vector for the raw data or a three-column matrix for the binned data.}

\item{ncomp}{a vector of two positive integers specifying the number of components of the
mixture model under the null and alternative hypothesis.
The first integer should be smaller than the second one. The default value is
\code{c(1, 2)}.}

\item{family}{a character string specifying the family of the mixture model, which can be one
of \code{normal}, \code{weibull}, \code{gamma}, or \code{lnorm} (default \code{normal}).}

\item{B}{the number of bootstrap iterations (default 100).}

\item{ev}{a logical value indicating whether the variance of each component should be the same
or not (default \code{FALSE} for \code{Normal} family and ignored for other family members).}

\item{mstep.method}{the method used in M-step of EM algorithm for \code{weibull} or
\code{gamma} family. It is ignored for \code{normal} or \code{lnorm} family,
which has closed-form solution in the M-step. The default value is \code{bisection}.}

\item{init.method}{a character string specifying the method used for providing the initial values
for the parameters for the EM algorithm. It can be one of \code{kmeans} or \code{hclust}. The default is
\code{kmeans}}

\item{tol}{the tolerance for the stopping rule of EM algorithm. It is the value to stop
EM algorithm when the two consecutive iterations produces log-likelihood with difference
less than \code{tol}. The default value is 1e-6.}

\item{max_iter}{the maximum number of iterations for the EM algorithm (default 500).}
}
\value{
The function \code{bs.test} returns an object of class \code{bootEM} which
contains the following three items.
\item{pvalue}{The p-value of the bootstrap likelihood ratio test}
\item{w0}{the observed likelihood ratio test statistic}
\item{w1}{a vector of simulated likelihood ratio test statistics}
}
\description{
This function performs the likelihood ratio test by parametric bootstrapping for two mixture
models with different number of components.
}
\details{
For the given data \code{x} and the specified family, the function \code{bs.test} conducts
a bootstrap likelihood ratio test for two mixture models with the number of components
under the null and the alternative hypothesis specified in \code{ncomp}.
}
\examples{
## testing normal mixture models with 2 and 3 components
set.seed(100)
x <- rmixnormal(200, c(0.5, 0.5), c(2, 5), c(1, 0.7))
ret <- bs.test(x, ncomp = c(2, 3), B = 30)
ret

## (not run) testing Weibull mixture models with 2 and 3 components
## set.seed(101)
## x <- rmixweibull(200, c(0.3, 0.4, 0.3), c(2, 5, 8), c(1, 0.6, 0.8))
## ret <- bs.test(x, ncomp = c(2, 3), family = "weibull", B = 30)
## ret

## (not run) testing Gamma mixture models with 1 and 2 components
## set.seed(102)
## x <- rgamma(200, 2, 1)
## ret <- bs.test(x, ncomp = c(1, 2), family = "gamma", B = 30)
## ret

}
\seealso{
\code{\link{plot.bootEM}}, \code{\link{mixfit}}, \code{\link{select}}
}
