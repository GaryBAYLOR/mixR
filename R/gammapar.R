#' Parameter Conversion for Gamma Distribution
#'
#' The function \code{to_mu_sd_gamma} converts the shape and rate to the mean and standard deviation
#'
#' The purpose of this function is to convert the parameterization of gamma distribution in the form of
#' shape and rate to the form of mean and standard deviation.
#'
#' @param alpha a numeric vector representing the shape of one or more than one gamma distributions
#' @param lambda a numeric vector representing the rate of one or more than one gamma distributions.
#' \code{alpha} and \code{lambda} should have the same length.
#'
#' @return a list of two items
#' \item{mu}{a vector of the means of gamma distributions}
#' \item{sd}{a vector of the standard deviations of gamma distributions}
#'
#' @seealso \code{\link{to_shape_rate_gamma}}
#'
#' @examples
#' to_mu_sd_gamma(2, 1)
#' to_mu_sd_gamma(c(2, 4), c(1, 1))
#'
#' @export
to_mu_sd_gamma <- function(alpha, lambda) {
	mu <- alpha / lambda
	sd <- sqrt(alpha) / lambda
	list(mu = mu, sd = sd)
}

#' Parameter Conversion for Gamma Distribution
#'
#' The function \code{to_shape_rate_gamma} converts the mean and standard deviation to the shape and rate
#'
#' The purpose of this function is to convert the parameterization of gamma distribution in the form of
#' mean and standard deviation to the form of shape and rate. It can be used for specifying the initial
#' values for the EM algorithm when the first-hand initial values are in the form of mean and standard
#' deviation from K-means clustering algorithm.
#'
#' @param mu a numeric vector representing the means of gamma distributions
#' @param sd a numeric vector representing the standard deviations of gamma distributions.
#' \code{mu} and \code{sd} should have the same length.
#'
#' @return a list of two items
#' \item{alpha}{a vector of the shapes of gamma distributions}
#' \item{lambda}{a vector of the rates of gamma distributions}
#'
#' @seealso \code{\link{to_mu_sd_gamma}}
#'
#' @examples
#' to_shape_rate_gamma(2, 1)
#' to_shape_rate_gamma(c(2, 4), c(1, 1))
#'
#' @export
to_shape_rate_gamma <- function(mu, sd) {
	alpha <- (mu / sd)^2
	lambda <- mu / sd^2
	list(alpha = alpha, lambda = lambda)
}
