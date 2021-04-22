#' Plot Bootstrap Likelihood Ratio Test
#'
#' This function is the plot method for the class \code{bootEM}.
#'
#' The histogram of the bootstrap LRT statistics \eqn{w_1} is plotted, with the
#' observed LRT statistic imposed in a red vertical line.
#' @param x an object of class \code{bootEM}, which is the output of the function \code{\link{bs.test}}.
#' @param ... the other parameters passed to the function \code{\link{hist}}
#'
#' @seealso \code{\link{bs.test}}
#'
#' @examples
#' ## plotting the bootstrap LRT result
#' set.seed(100)
#' x <- rmixnormal(200, c(0.5, 0.5), c(2, 5), c(1, 0.7))
#' ret <- bs.test(x, ncomp = c(2, 3), B = 30)
#' plot(ret)
#'
#' @export
plot.bootEM <- function(x,...) {
	xlim <- range(c(x$w0, x$w1), na.rm = TRUE)
	hist(x$w1, xlim = xlim, breaks = 20, ...)
	abline(v = x$w0, col = "red", lwd = 2)
}
