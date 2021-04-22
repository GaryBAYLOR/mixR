#' Plotting the Fitted Mixture Models
#'
#' This is the plot method for the class \code{mixfitEM}. It is used to plot the fitted mixture models
#' by using base R plotting system or using the package ggplot2.
#'
#' The function \code{plot.mixfitEM} is used for plotting an object of class \code{mixfitEM}, which is
#' an output of the function \code{\link{mixfit}}. Users can choose base R plotting system or ggplot2
#' (the package ggplot2 needs to be installed).
#' plotting system. The plot is a density plot of the fitted mixture model imposed on top of a histogram.
#' The parameters that control the appearance of the histogram and the density curve can be changed.
#' The density curve of each component can be shown or hidden.
#'
#' @param x an object of class \code{mixfitEM}, usually an output from the function \code{\link{mixfit}}
#' @param ps a character string to select the plotting system, which can be \code{base} (default),
#' or \code{ggplot2}
#' @param detail a logical value controlling whether to show each component of the fitted mixture model
#' (default \code{TRUE})
#' @param smoothness a positive integer controlling the smoothness of the density curve in the plot.
#' The default value is 512 and increasing this value will produce smoother curve.
#' @param ... the other parameters controlling the appearance of the plot, which are the following
#' parameters if we specify family as \code{base}:
#' \describe{
#' \item{xlim}{a numeric vector of length 2 specifying the range of x-axis of the plot}
#' \item{ylim}{a numeric vector of length 2 specifying the range of y-axis of the plot}
#' \item{lty}{the line type of the mixture density curve, default 1}
#' \item{lwd}{the line width of the mixture density curve, default 2}
#' \item{color}{the line color of the mixture density curve, default "black"}
#' \item{...}{arguments passed to \code{\link{hist}}}
#' }
#' or the following parameters if we specify family as \code{ggplot2}:
#' \describe{
#' \item{xlim}{a numeric vector of length 2 specifying the range of x-axis of the plot}
#' \item{ylim}{a numeric vector of length 2 specifying the range of y-axis of the plot}
#' \item{theme}{the background of the plot, can be "grey" or "bw" (default "grey")}
#' \item{trans}{the transparency of the plot, default 0.5}
#' \item{...}{arguments passed to \code{\link[ggplot2]{geom_path}}}
#' }
#'
#' @seealso \code{\link{mixfit}}
#'
#' @examples
#' x <- rmixnormal(200, c(0.3, 0.7), c(2, 5), c(1, 0.7))
#' fit <- mixfit(x, ncomp = 2)
#' plot(fit)  # base R plotting system
#' plot(fit, "ggplot2")  # ggplot2 plotting system
#'
#' @export
#' @import ggplot2
plot.mixfitEM <- function(x, ps = c("base", "ggplot2"), detail = TRUE, smoothness = 512, ...) {
	ps <- match.arg(ps)
	if(ps == "ggplot2") {
		requireNamespace("ggplot2")
	}
	family <- x$family
	#args <- mget(names(formals()),sys.frame(sys.nframe()))
	mc <- match.call()
	mc$ps <- NULL
	mc$detail <- detail
	mc$smoothness <- smoothness

	# fun.name <- ifelse(ps == "base", paste0("plot", family), paste0("ggplot", family))
	if(ps == "base") {
	   fun.name <- paste0("plot", family)
	   mc[[1]] <- as.name(fun.name)
	 } else {
	   fun.name <- paste0("ggplot", family)
	   mc[[1]] <- as.name(fun.name)
	   names(mc)[2] <- "object"
	 }
	 eval(mc, environment())
}





