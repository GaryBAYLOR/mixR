#' Bootstrap Likelihood Ratio Test for Finite Mixture Models
#'
#' This function performs the likelihood ratio test by parametric bootstrapping for two mixture
#' models with different number of components.
#'
#' For the given data \code{x} and the specified family, the function \code{bs.test} conducts
#' a bootstrap likelihood ratio test for two mixture models with the number of components
#' under the null and the alternative hypothesis specified in \code{ncomp}.
#'
#' @param x a numeric vector for the raw data or a three-column matrix for the binned data.
#' @param ncomp a vector of two positive integers specifying the number of components of the
#' mixture model under the null and alternative hypothesis.
#' The first integer should be smaller than the second one. The default value is
#' \code{c(1, 2)}.
#' @param family a character string specifying the family of the mixture model, which can be one
#'  of \code{normal}, \code{weibull}, \code{gamma}, or \code{lnorm} (default \code{normal}).
#' @param B the number of bootstrap iterations (default 100).
#' @param ev a logical value indicating whether the variance of each component should be the same
#' or not (default \code{FALSE} for \code{Normal} family and ignored for other family members).
#' @param mstep.method the method used in M-step of EM algorithm for \code{weibull} or
#' \code{gamma} family. It is ignored for \code{normal} or \code{lnorm} family,
#' which has closed-form solution in the M-step. The default value is \code{bisection}.
#' @param init.method a character string specifying the method used for providing the initial values
#' for the parameters for the EM algorithm. It can be one of \code{kmeans} or \code{hclust}. The default is
#' \code{kmeans}
#' @param tol the tolerance for the stopping rule of EM algorithm. It is the value to stop
#' EM algorithm when the two consecutive iterations produces log-likelihood with difference
#' less than \code{tol}. The default value is 1e-6.
#' @param max_iter the maximum number of iterations for the EM algorithm (default 500).
#'
#' @return The function \code{bs.test} returns an object of class \code{bootEM} which
#' contains the following three items.
#' \item{pvalue}{The p-value of the bootstrap likelihood ratio test}
#' \item{w0}{the observed likelihood ratio test statistic}
#' \item{w1}{a vector of simulated likelihood ratio test statistics}
#'
#' @seealso \code{\link{plot.bootEM}}, \code{\link{mixfit}}, \code{\link{select}}
#'
#' @examples
#' ## testing normal mixture models with 2 and 3 components
#' set.seed(100)
#' x <- rmixnormal(200, c(0.5, 0.5), c(2, 5), c(1, 0.7))
#' ret <- bs.test(x, ncomp = c(2, 3), B = 30)
#' ret
#'
#' ## (not run) testing Weibull mixture models with 2 and 3 components
#' ## set.seed(101)
#' ## x <- rmixweibull(200, c(0.3, 0.4, 0.3), c(2, 5, 8), c(1, 0.6, 0.8))
#' ## ret <- bs.test(x, ncomp = c(2, 3), family = "weibull", B = 30)
#' ## ret
#'
#' ## (not run) testing Gamma mixture models with 1 and 2 components
#' ## set.seed(102)
#' ## x <- rgamma(200, 2, 1)
#' ## ret <- bs.test(x, ncomp = c(1, 2), family = "gamma", B = 30)
#' ## ret
#'
#' @export
bs.test <- function(x, ncomp = c(1, 2), family = c("normal", "weibull", "gamma", "lnorm"),
B = 100, ev = FALSE, mstep.method = c("bisection", "newton"), init.method = c("kmeans", "hclust"),
tol = 1e-6, max_iter = 500) {

	family <- match.arg(family)
	CheckData(x, family)

	  mstep.method <- match.arg(mstep.method)
	  init.method <- match.arg(init.method)
    mc <- match.call()
    mc$family <- NULL
    mc$B <- NULL
    mc$ev <- ev
    mc$mstep.method <- mstep.method
    mc$init.method <- init.method
    mc$tol <- tol
	mc$max_iter <- max_iter

	fun.name <- ifelse(!is.matrix(x), paste0(family, "EM"), paste0(family, "EM2"))
	mc[[1]] <- as.name(fun.name)

	mc1 <- mc
	mc2 <- mc
  
	if(!is.vector(ncomp, mode = 'numeric')) {
	  stop("'ncomp' must be a numeric vector!")
	} else if (length(ncomp) != 2) {
	  stop("'ncomp' must be of length two!")
	}
	ncomp <- as.integer(ncomp)
	if (any(is.na(ncomp))) {
	  stop("'ncomp' must not contain missing value!")
	} else if (ncomp[1] >= ncomp[2]) {
	  stop("ncomp[1] must be less than ncomp[2] after coercion to integers!")
	} else if (ncomp[1] < 1) {
	  stop("'ncomp' must be positive!")
	} else if (ncomp[2] > 20) {
	  stop("'ncomp' larger than 20 is currently not supported!")
	}
	
	mc1$ncomp <- ncomp[1]
	mc2$ncomp <- ncomp[2]

	fit1 <- eval(mc1, environment())
	fit2 <- eval(mc2, environment())
	w0 <- (-2) * (fit1$loglik - fit2$loglik)
	w1 <- numeric(B)
	pi <- fit1$pi
	mu <- fit1$mu
	sd <- fit1$sd

	i <- 1
	if(is.matrix(x)) {
		n <- sum(x[, 3])
		nr <- nrow(x)
		brks <- c(x[, 1], x[nr, 2])
		while(i <= B) {
			x_new <- do.call(paste0("rmix", family), list(n, pi, mu, sd))
			x_new <- bin(x_new, brks)
			mc1$x <- x_new
		    mc2$x <- x_new
		    fit1x <- eval(mc1, environment())
		    fit2x <- eval(mc2, environment())
		    w1[i] <- (-2) * (fit1x$loglik - fit2x$loglik)
		    if((!is.na(w1[i])) && (!is.nan(w1[i])) && w1[i] >= 0) i <- i + 1
		}
	} else {
		n <- length(x)
		while(i <= B) {
		    x_new <- do.call(paste0("rmix", family), list(n, pi, mu, sd))
		    mc1$x <- x_new
		    mc2$x <- x_new
		    fit1x <- eval(mc1, environment())
		    fit2x <- eval(mc2, environment())
		    w1[i] <- (-2) * (fit1x$loglik - fit2x$loglik)
		    if((!is.na(w1[i])) && (!is.nan(w1[i])) && w1[i] >= 0) i <- i + 1
	    }
	}
	pvalue <- mean(w1 > w0, na.rm = TRUE)
	res <- list(pvalue = pvalue, w0 = w0, w1 = w1)
	class(res) <- "bootEM"
	return(res)
}










