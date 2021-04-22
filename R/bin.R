#' Binning the Raw Data
#'
#' This function creates a binned data from a numeric vector
#'
#' Given a numeric vector, the function \code{bin} creates binned data with bin value provided
#' by \code{brks}. Fitting mixture models with a large dataset may be slow, especially when
#' we want to fit a mixture model outside of normal family. Binning the data with a relatively
#' small binwidth speeds up the computation of EM algorithm while at the same time keeps the
#' precision of the estimation result.
#'
#' @param x a numeric vector
#' @param brks a numeric vector in increasing order, representing the value of each bin
#' @return The function \code{bin} returns a matrix with three columns, representing the value of the left bin, the value of the right bin and the number of observations in \code{x} that falls in each bin.
#'
#' @seealso \code{\link{reinstate}}
#'
#' @examples
#' set.seed(99)
#' x <- rmixnormal(200, c(0.5, 0.5), c(2, 5), c(1, 1))
#' data <- bin(x, seq(-2, 10, 0.1))
#' fit1 <- mixfit(x, ncomp = 2)
#' fit2 <- mixfit(data, ncomp = 2)
#'
#' @export
bin <- function(x, brks) {
	k <- length(brks)
	res <- matrix(NA, nrow = k-1, ncol = 3)
	colnames(res) <- c("a", "b", "freq")
	res[,1] <- brks[-k]
	res[,2] <- brks[-1]
	temp <- .bincode(x, brks)
	temp <- temp[!is.na(temp)]

	for(i in 1:(k-1)) {
		res[i, 3] <- sum(temp == i)
	}
	res[res[, 3] != 0, ]
}
