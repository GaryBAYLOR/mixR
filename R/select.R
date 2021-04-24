#' Finite Mixture Model Selection by Information Criterion
#'
#' This function selects the best model from a candidate of mixture models based on the
#' information criterion BIC.
#'
#' By specifying different number of components, the function \code{select} fits a series
#' of mixture models for a given family, and a mixture model with minimum value of BIC
#' is regarded as the best.
#'
#' @param x a numeric vector for raw data or a three-column matrix for the binned data
#' @param ncomp a vector of positive integers specifying the number of components of the candidate mixture models
#' @param family a character string specifying the family of the mixture model. It can only be
#' one element from \code{normal}, \code{weibull}, \code{gamma} or \code{lnorm}.
#' @param mstep.method a character string specifying the method used in M-step of the EM algorithm
#' when fitting weibull or gamma mixture models. It can be either \code{bisection} or \code{newton}.
#' The default is \code{bisection}.
#' @param init.method a character string specifying the method used for providing initial values
#' for the parameters for EM algorithm. It can be one of \code{kmeans} or \code{hclust}. The default is
#' \code{kmeans}
#' @param tol the tolerance for the stopping rule of EM algorithm. It is the value to stop EM algorithm when the two
#' consecutive iterations produces loglikelihood with difference less than \code{tol}. The default value is 1e-6.
#' @param max_iter the maximum number of iterations for the EM algorithm (default 500).
#'
#' @return The function returns an object of class \code{selectEM} which contains the following items.
#'         \item{ncomp}{the specified number of components of the candidate mixture models}
#'         \item{equal.var}{a logical vector indicating whether the variances of each component in each mixture model
#'         are constrained to be the same (only for \code{normal} family)}
#'         \item{bic}{the value of BIC for each mixture model}
#'         \item{best}{an indicator of the best model}
#'         \item{family}{the family of the mixture model}
#'
#' @seealso \code{\link{plot.selectEM}}, \code{\link{bs.test}}, \code{\link{mixfit}}
#'
#' @examples
#' ## selecting the optimal normal mixture model by BIC
#' set.seed(105)
#' x <- rmixnormal(1000, c(0.3, 0.4, 0.3), c(-4, 0, 4), c(1, 1, 1))
#' hist(x, breaks = 40)
#' ret <- select(x, ncomp = 2:5)
#' ## [1] "The final model: normal mixture (equal variance) with 3 components"
#'
#' ## (not run) selecting the optimal Weibull mixture model by BIC
#' ## set.seed(106)
#' ## x <- rmixweibull(1000, c(0.3, 0.4, 0.3), c(2, 5, 8), c(0.7, 0.6, 1))
#' ## ret <- select(x, ncomp = 2:5, family = "weibull")
#' ## [1] "The final model: weibull mixture with 3 components"
#'
#' ## (not run) selecting the optimal Gamma mixture model by BIC
#' ## set.seed(107)
#' ## x <- rmixgamma(1000, c(0.3, 0.7), c(2, 5), c(0.7, 1))
#' ## ret <- select(x, ncomp = 2:5, family = "gamma")
#' ## [1] "The final model: gamma mixture with 2 components"
#'
#'
#' ## (not run) selecting the optimal lognormal mixture model by BIC
#' ## set.seed(108)
#' ## x <- rmixlnorm(1000, c(0.2, 0.3, 0.2, 0.3), c(4, 7, 9, 12), c(1, 0.5, 0.7, 1))
#' ## ret <- select(x, ncomp = 2:6, family = "lnorm")
#' ## [1] "The final model: lnorm mixture with 4 components"
#'
#' @export
select <- function(x, ncomp, family = c("normal", "weibull", "gamma", "lnorm"),
mstep.method = c("bisection", "newton"), init.method = c("kmeans", "hclust"),
tol = 1e-6, max_iter = 500) {
  family <- match.arg(family)
  CheckData(x, family)
	mstep.method <- match.arg(mstep.method)
	init.method <- match.arg(init.method)
	mc <- match.call()
	mc$family <- NULL
	mc$mstep.method <- mstep.method
	mc$init.method <- init.method
	#mc$ev <- ev
	mc$tol <- tol
	mc$max_iter <- max_iter
    fun.name <- ifelse(!is.matrix(x), paste0(family, "EM"), paste0(family, "EM2"))
    mc[[1]] <- as.name(fun.name)

    if(family == "normal") {
        k <- length(ncomp)
	    col1 <- rep(ncomp, each = 2)
	    col2 <- rep(c("Y", "N"), k)
	    bic <- numeric(2 * k)
	    best <- rep(" ", 2 * k)

        for(i in (1:(2 * k))) {
    	        mc$ncomp <- col1[i]
    	        mc$ev <- (i %% 2 == 1)
	        tmp <- eval(mc, environment())
	        bic[i] <- tmp$bic
	    }

	    index <- which.min(bic)
	    best[index] <- "*"
	    res <- list(ncomp = col1, equal.var = col2, bic = bic, best = best, family = family)
	    if(col2[index] == "Y") {
		    text <- paste("The final model: normal mixture (equal variance) with",
		    col1[index], "components", sep = " ")
	    } else {
		    text <- paste("The final model: normal mixture (unequal variances) with",
		    col1[index], "components", sep = " ")
	    }
	} else {
		k <- length(ncomp)
	    col1 <- ncomp
	    bic <- numeric(k)
	    best <- rep(" ", k)

		for(i in 1:k) {
    	   mc$ncomp <- col1[i]
	       tmp <- eval(mc, environment())
	       bic[i] <- tmp$bic
	    }

	    index <- which.min(bic)
	    best[index] <- "*"
	    res <- list(ncomp = col1, bic = bic, best = best, family = family)
	    text <- paste("The final model:", family, "mixture with", col1[index], "components", sep = " ")
	}
	print(text)
    class(res) <- "selectEM"
	invisible(res)
}












