#' Reinstate the Binned Data to the Raw Data
#'
#' This function creates a numeric vector approximating the raw data from binned data
#'
#' The function \code{reinstate} creates a numeric vector by generating \eqn{n_i}
#' random data from the Uniform distribution \eqn{U(a_i, b_i)} for \eqn{i = 1, \dots, r}
#' and then combine all random data together. \eqn{a_i, b_i, n_i}
#' are the first, second and the third column of the matrix \code{data}
#' and \eqn{r} is the number of bins.
#' It is used for enabling parameter initialization for EM algorithm when we fit mixture
#' models for binned data.
#'
#' @param data a three-column matrix representing the raw data
#' @return The function returns a numeric vector.
#'
#' @seealso \code{\link{bin}}
#'
#' @examples
#' x <- rnorm(100)
#' data <- bin(x, seq(-3, 3, 0.25))
#' y <- reinstate(data)
#'
#' @export
reinstate <- function(data) {
	rdata <- numeric()
	for(i in 1:nrow(data)) {
		rmin <- data[i, 1]
		rmax <- data[i, 2]
		r_num <- data[i, 3]
		rdata <- c(rdata, runif(r_num, rmin, rmax))
	}
	rdata
}
