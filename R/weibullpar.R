#' Parameter Conversion for Weibull Distribution
#'
#' The function \code{to_mu_sd_weibull} converts the parameters of shape and scale of weibull distributions to
#' the parameters of the mean and standard deviation.
#'
#' The purpose of this function is to convert the parameterization of Weibull distribution in the form of
#' shape and scale to the form of mean and standard deviation.
#' @param k a numeric vector representing the shape of a series of Weibull distributions
#' @param lambda a numeric vector representing the scale of a series of Weibull distributions.
#' \code{k} and \code{lambda} should have the same length.
#'
#' @return a list of two items
#' \item{mu}{a vector of the means of Weibull distributions}
#' \item{sd}{a vector of the standard deviations of Weibull distributions}
#'
#' @seealso \code{\link{to_k_lambda_weibull}}
#'
#' @examples
#' to_mu_sd_weibull(2, 1)
#' to_mu_sd_weibull(c(2, 4), c(1, 1))
#'
#' @export
to_mu_sd_weibull <- function(k, lambda) {
	mu <- lambda * gamma(1 + 1/k)
	var <- lambda^2 * (gamma(1 + 2/k) - gamma(1 + 1/k)^2)
	sd <- sqrt(var)
	list(mu  = mu, sd = sd)
}

#' Parameter Conversion for Weibull Distribution
#'
#' The function \code{to_k_lambda_weibull} converts the mean and standard deviation to the shape and scale for
#' the Weibull distributions.
#'
#' The purpose of this function is to convert the parameterization of Weibull distribution in the form of
#' mean and standard deviation to the form of shape and scale. It can be used for specifying the initial
#' values for the EM algorithm when the first-hand initial values are in the form of mean and standard
#' deviation from K-means clustering algorithm.
#'
#' @param mu a numeric vector representing the means of Weibull distributions
#' @param sd a numeric vector representing the standard deviations of Weibull distributions.
#' \code{mu} and \code{sd} should have the same length.
#'
#' @return a list of two items
#' \item{k}{a vector of the shapes of Weibull distributions}
#' \item{lambda}{a vector of the scales of Weibull distributions}
#'
#' @seealso \code{\link{to_mu_sd_weibull}}
#'
#' @examples
#' to_k_lambda_weibull(2, 1)
#' to_k_lambda_weibull(c(2, 5), c(1, 0.7))
#'
#' @export
to_k_lambda_weibull <- function(mu, sd) {
  to_k_lambda_weibull_inner <- function(mu, sd) {
    delta <- (mu / sd)^2
    f <- function(z) {
      (1 + delta) * gamma(z)^2 - delta * gamma(2 * z -1)
    }
    
    # initial values for z_right (z_left is kept at 1 as f(1) is always 1)
    z_right <- 2
    while (f(z_right) > 0) z_right <- z_right * 2
    
    # find root of f(z) = 0
    z_root <- uniroot(f, c(1, z_right))$root
    k <- 1 / (z_root - 1)
    lambda <- mu / gamma(z_root)
    
    list(k = k, lambda = lambda)
  }
  k <- lambda <- numeric(length(mu))
  for(i in 1:length(mu)) {
    res <- to_k_lambda_weibull_inner(mu[i], sd[i])
    k[i] <- res$k
    lambda[i] <- res$lambda
  }
  
  list(k = k, lambda = lambda)
}
