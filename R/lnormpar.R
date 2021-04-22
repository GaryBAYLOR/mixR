#' Parameter Conversion for Lognormal Distribution
#'
#' The function \code{to_mu_sd_lnorm} converts the logarithm mean and logarithm standard deviation to the
#' mean and standard deviation
#'
#' The purpose of this function is to convert the parameterization of lognormal distribution in the
#' form of logarithm mean and logarithm standard deviation to the form of mean and standard deviation.
#'
#' @param mulog a vector of logarithm means of lognormal distributions
#' @param sdlog a vector of logarithm standard deviations of lognormal distributions
#'
#' @return a list of two items
#' \item{mu}{a vector of the means of lognormal distributions}
#' \item{sd}{a vector of the standard deviations of lognormal distributions}
#'
#' @seealso  \code{\link{to_mulog_sdlog_lnorm}}
#'
#' @examples
#' to_mu_sd_lnorm(2, 1)
#' to_mu_sd_lnorm(c(2, 4), c(1, 1))
#'
#' @export
to_mu_sd_lnorm <- function(mulog, sdlog) {
	mu <- exp(mulog + sdlog^2 / 2)
	var <- (exp(sdlog^2) - 1) * exp(2 * mulog + sdlog^2)
	sd <- sqrt(var)
	list(mu = mu, sd = sd)
}

#' Parameter Conversion for Lognormal Distribution
#'
#' The function \code{to_mulog_sdlog_lnorm} converts the mean and standard deviation to
#' the logarithm mean and logarithm standard deviation
#'
#' The purpose of this function is to convert the parameterization of lognormal distribution in the
#' form of mean and standard deviation to the form of logarithm mean and logarithm standard deviation.
#' It can be used for specifying the initial values for the EM algorithm when the first-hand intial values
#' are in the form of mean and standard deviation from K-means clustering algorithm.
#'
#' @param mu a vector of means of lognormal distributions
#' @param sd a vector of standard deviations of lognormal distributions
#'
#' @return a list of two items
#' \item{mulog}{a vector of lognormal means of lognormal distributions}
#' \item{sdlog}{a vector of lognormal standard deviations of lognormal distributions}
#'
#' @seealso \code{\link{to_mu_sd_lnorm}}
#'
#' @examples
#' to_mulog_sdlog_lnorm(2, 1)
#' to_mulog_sdlog_lnorm(c(2, 4), c(1, 1))
#'
#' @export
to_mulog_sdlog_lnorm <- function(mu, sd) {
	tmp <- log((sd / mu)^2 + 1)
	mulog <- log(mu) - 0.5 * tmp
	sdlog <- sqrt(tmp)
	list(mulog = mulog, sdlog = sdlog)
}
