#' Print Method for Class \code{mixfitEM}
#'
#' This function is the print method for the \code{mixfitEM} class.
#'
#' \code{print.mixfitEM} prints the value of the parameters of a fitted mixture model, together
#' with some other information like the number of iterations of the EM algorithm, the loglikelihood,
#' the value of AIC and BIC.
#'
#' @param x an object of class \code{mixfitEM}
#' @param digits the digits to print for the values in the print output. The default
#' value is from the global option \code{getOption("digits")}.
#' @param ... other arguments passed to \code{print}
#'
#' @seealso \code{\link{mixfit}}
#'
#' @examples
#' x <- rmixnormal(200, c(0.5, 0.5), c(2, 5), c(1, 0.7))
#' fit <- mixfit(x, ncomp = 2)
#' print(x)
#'
#' @export
print.mixfitEM <- function(x, digits = getOption("digits"), ...) {
	family <- x$family
	#args <- mget(names(formals()),sys.frame(sys.nframe()))
	mc <- match.call()
	mc$digits <- digits
	fun.name <- paste0("print", family)
	mc[[1]] <- as.name(fun.name)
	eval(mc, environment())
}

