#' Generating Random Data From A Normal Mixture Model
#'
#' The function \code{rmixnormal} generates random data from a normal mixture model.
#'
#' The number of random data from each component \eqn{n_0} (a vector) is generated from a multinomial
#' distribution Multinom\eqn{(n, pi)}. Then the random data from each component is generated with
#' the sample sized specified in \eqn{n_0} and parameters of normal distributions specified in
#' \code{mu} and \code{sd}.
#'
#' @param n a positive integer specifying the number of observations we want to generate from the mixture model
#' @param pi a numeric vector for the proportion of each component
#' @param mu a numeric vector for the mean of each component
#' @param sd a numeric vector for the standard deviation of each component
#'
#' @return The function \code{rmixnormal} returns a numeric vector of random data from the specified normal mixture model.
#'
#' @seealso \code{\link{rmixweibull}}, \code{\link{rmixgamma}}, \code{\link{rmixlnorm}}
#'
#' @examples
#' x <- rmixnormal(1000, c(0.4, 0.6), c(2, 5), c(1, 0.5))
#' hist(x, breaks = 40)
#'
#' @export
rmixnormal <- function(n, pi, mu, sd) {
	check_inputs_for_rmix(n, pi, mu, sd)
	res <- numeric(0)
	n0 <- rmultinom(1, n, pi)[, 1]
	for(i in 1:length(pi)) {
		tmp <- rnorm(n0[i], mu[i], sd[i])
		res <- c(res, tmp)
	}
	res
}
