#' Plot Method for Class \code{selectEM}
#'
#' This function plots the result of mixture model selection by BIC.
#'
#' The function \code{plot.selectEM} is the plot method for the class \code{selectEM}. It plots
#' the number of components against the corresponding value of BIC. It is used to visually display
#' the mixture model selection result by BIC.
#' @param x an object of class \code{selectEM}, which is an output of the function \code{\link{select}}.
#' @param leg.loc the location of the legend, which is the same as the first argument of the function
#' @param ... other arguments passed to \code{plot}
#' \code{\link{legend}}. The default value is "topright". The user can change its location
#' (to "topleft", "bottom right" etc.) if the visual plot conflicts with the legend.
#'
#' @seealso \code{\link{select}}
#'
#' @examples
#' x <- rmixnormal(200, c(0.3, 0.7), c(2, 5), c(1, 1))
#' res <- select(x, ncomp = 1:3)
#' plot(res)
#'
#' @export
plot.selectEM <- function(x, leg.loc = "topright", ...) {
	if(x$family == "normal") {
		len <- length(x$ncomp)
	    s1 <- seq(1, len, by = 2)
	    s2 <- seq(2, len, by = 2)
	    plot(x$ncomp[s1], x$bic[s1], type = "b", ylim = c(min(x$bic), max(x$bic)),
	    main = "Model selection by BIC", xlab = "Number of components", ylab = "BIC", ...)
	    lines(x$ncomp[s2], x$bic[s2], type = "b", col = "blue")
	    legend(leg.loc, lty = c(1, 1), col = c("black", "blue"), legend = c("EV", "UV"), bty = "n")
	} else {
		plot(x$ncomp, x$bic, type = "b", ylim = c(min(x$bic), max(x$bic)),
	    main = paste(x$family, "mixture model selection by BIC"), xlab = "Number of components", ylab = "BIC")
	}
}

