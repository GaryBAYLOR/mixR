#' Initialization of the EM Algorithm
#'
#' This function returns the mean and standard deviation of each component by using
#' K-means clustering or hierarchical clustering.
#'
#' The function \code{initz} returns the mean and standard deviation of each component
#' of a mixture model by using K-means clustering algorithm, or hierarchical clustering
#' method. It is used for automatically selecting initial values for the EM algorithm,
#' so as to enable mixture model selection by bootstrapping likelihood ratio test or
#' using information criteria.
#'
#' @param x a numeric vector of the raw data or a three-column matrix of the binned data
#' @param ncomp a positive integer specifying the number of components for a mixture model
#' @param init.method the method used for providing initial values, which can be one of
#' \code{kmeans} or \code{hclust}.
#' @return \code{initz} returns a list with three items
#' \item{pi }{a numeric vector of component proportions}
#' \item{mu }{a numeric vector of component means}
#' \item{sd }{a numeric vector of component standard deviations}
#' @examples
#' x <- rmixnormal(500, c(0.5, 0.5), c(2, 5), c(1, 0.7))
#' data <- bin(x, seq(-2, 8, 0.25))
#' par1 <- initz(x, 2)
#' par2 <- initz(data, 2)
#'
#' @export
initz <- function(x, ncomp, init.method = c("kmeans", "hclust")) {

	init.method = match.arg(init.method)
	# check if 'x' is a matrix (from grouped data)
	if(is.matrix(x)) {
		x <- reinstate(x)
	}
	if(init.method == "kmeans") {
		a <- kmeans(x, centers = ncomp, nstart = 3)$cluster
	} else {
		a <- cutree(hclust(dist(x)), ncomp)
	}
	res <- list()
	for(i in 1:ncomp) {
		res[[i]] <- x[a == i]
	}
	count <- sapply(res, length)
	pi <- count / sum(count)
	mu <- sapply(res, mean)
	sd <- sapply(res, sd)
	sd[is.na(sd)] = 0.1
	order <- order(mu)

	pi <- pi[order]
	mu <- mu[order]
	sd <- sd[order]
	list(pi = pi, mu = mu, sd = sd)
}
