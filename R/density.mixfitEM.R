#' The Density of Finite Mixture Models
#'
#' This function calculates the probability density of a finite mixture model.
#'
#' The function \code{density.mixfitEM} is the method of the generic function
#' \code{density} for the class \code{mixfitEM}.
#'
#' @param x an object of class \code{mixfitEM}
#' @param at a scalar or a numeric vector of locations where densities are calculated
#' @param smoothness a positive integer controlling the smoothness of the density curve (default 512).
#' The higher this value is, the more locations of the mixture model the density is calculated.
#' @param cut the number of standard deviations away the density is to be computed (default 3.8)
#' @param ... other arguments
#' 
#' @return This function returns a list of class \code{densityEM}, which contains the following
#' items.
#' \item{x}{a scalar or a numeric vector of locations where densities are calculated.}
#' \item{y}{a vector of the densities of the mixture model at the corresponding locations in \code{x}}
#' \item{comp}{a matrix with columns representing the densities of each component in the mixture model at the corresponding locations in \code{x}}
#' 
#' @seealso \code{\link{mixfit}}
#'
#' @examples
#' set.seed(102)
#' x <- rmixnormal(200, c(0.5, 0.5), c(2, 5), c(1, 0.7))
#' fit1 <- mixfit(x, ncomp = 2)
#' d1 = density(fit1)
#' d2 = density(fit1, at = 0)
#'
#' @export
#' @import graphics
#' @import stats

density.mixfitEM = function(x, at, smoothness = 512, cut = 3.8, ...) {
  mc <- match.call()
  family <- x$family
  mc$smoothness <- smoothness
  mc$cut <- cut
  mc[[1]] <- as.name(paste0("density", family))
  eval(mc, envir = environment())
}

