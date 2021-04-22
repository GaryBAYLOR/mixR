#' Print Method for Class \code{selectEM}
#'
#' The function prints the result of mixture model selection.
#'
#' The function \code{print.selectEM} is the print method for the class{selectEM}, which is the output
#' of the function \code{select}. It prints a data frame which contains the following information of each
#' candidate mixture models: the number of components, whether the variance is the same for each component
#' in a mixture model (only for normal), the value of BIC, and an indicator of the best model.
#'
#' @param x an object of class \code{selectEM}
#' @param ... other arguments passed to \code{print}
#'
#' @seealso \code{\link{select}}
#'
#' @export
print.selectEM <- function(x, ...) {
	family <- x$family
	if(family == "normal") {
		res <- data.frame(ncomp = x[[1]],equal.var = x[[2]],bic = x[[3]], best = x[[4]])
	} else {
		res <- data.frame(ncomp = x[[1]], bic = x[[2]], best = x[[3]])
	}
	print(res)
}
