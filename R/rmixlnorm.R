#' Generating Random Data From A Lognormal Mixture Model
#'
#' The function \code{rmixlnorm} generates random data from a lognormal mixture model.
#'
#' The number of random data from each component \eqn{n_0} (a vector) is generated from a multinomial
#' distribution Multinom\eqn{(n, pi)}. Then the random data from each component is generated with
#' the sample sized specified in \eqn{n_0} and parameters of lognormal distributions specified in
#' \code{mu} and \code{sd}.
#'
#' @param n a positive integer specifying the number of observations we want to generate from the mixture model
#' @param pi a numeric vector for the proportion of each component
#' @param mu a numeric vector for the mean of each component
#' @param sd a numeric vector for the standard deviation of each component
#'
#' @return The function \code{rmixlnorm} returns a numeric vector of random data from the specified lognormal mixture model.
#'
#' @seealso \code{\link{rmixnormal}}, \code{\link{rmixweibull}}, \code{\link{rmixgamma}}
#'
#' @examples
#' x <- rmixlnorm(1000, c(0.4, 0.6), c(2, 5), c(1, 0.5))
#' hist(x, breaks = 40)
#'
#' @export
rmixlnorm <- function(n, pi, mu, sd) {
  check_inputs_for_rmix(n, pi, mu, sd)
  if(any(mu <= 0)) stop("'mu' should be positive!")

	n0 <- rmultinom(1, n, pi)[, 1]
	pars <- to_mulog_sdlog_lnorm(mu, sd)
	mulog <- pars[[1]]
	sdlog <- pars[[2]]
	res <- numeric(0)
	for(i in 1:length(pi)) {
		tmp <- rlnorm(n0[i], mulog[i], sdlog[i])
		res <- c(res, tmp)
	}
	res
}
