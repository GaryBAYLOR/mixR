#' The Density of Finite Mixture Models
#'
#' This function calculates the probability density of a finite mixture model.
#'
#' The function \code{density.mixfitEM} is the method of the generic function
#' \code{density} for the class \code{mixfitEM}.
#'
#' @param x an object of class \code{mixfitEM}
#' @param smoothness a positive integer controlling the smoothness of the density curve (default 512).
#' The higher this value is, the more locations of the mixture model the density is calculated.
#' @param from the starting location the density is going to be calculated
#' @param to the ending location the density is going to be computed
#' @param cut the number of standard deviations away the density is to be computed (default 3.5)
#' @param ... other arguments passed to \code{\link[stats]{density}}
#' @return This function returns a list of class \code{density}, which contains the following
#' items.
#' \item{x}{a numeric vector of locations where density is calculated.}
#' \item{y}{the density of the mixture model at the corresponding locations in \code{x}}
#'
#' @seealso \code{\link{mixfit}}
#'
#' @examples
#' set.seed(102)
#' x <- rmixnormal(200, c(0.5, 0.5), c(2, 5), c(1, 0.7))
#' fit1 <- mixfit(x, ncomp = 2)
#' fit2 <- mixfit(x, ncomp = 2, ev = TRUE)
#' plot(fit1, detail = FALSE, breaks = 20)
#' lines(density(fit2), col = "red")
#'
#' @export
#' @import graphics
#' @import stats
density.mixfitEM <- function(x, smoothness = 512, from = NULL, to = NULL, cut = 3.5, ...) {
	mc <- match.call()
	family <- x$family
	mc$smoothness <- smoothness
	mc$cut <- cut
	mc[[1]] <- as.name(paste0("density", family))
	eval(mc, envir = environment())
}


