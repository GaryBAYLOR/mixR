#' Initialization of EM Algorithm
#'
#' This function returns the mean and standard deviation of each component by using
#' K-means clustering method or hierarchical clustering method.
#'
#' The function \code{initz} returns the mean and standard deviation of each component
#' of a mixture model by using K-means clustering algorithm, or hierarchical clustering
#' method. It is used for automatically selecting initial values for the EM algorithm,
#' so as to enable mixture model selection by bootstrapping likelihood ratio test or
#' using information criteria.
#'
#' @param x a numeric vector for raw data or a three-column matrix for binned data
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
initz = function (x, ncomp, init.method = c("minibatch-kmeans", "kmeans", "hclust")) 
{
  init.method = match.arg(init.method)
  if (is.matrix(x)) {
    x <- reinstate(x)
  }
  
  n = length(x)
  if (n > 1e6) {
    x <- sample(x, 2e5 + (n - 2e5) * 0.01)
  } else if (n > 1e5) {
    x <- sample(x, 1e5 + (n - 1e5) * 0.1)
  }
  
  if(init.method == "minibatch-kmeans") {
    fit <- MiniBatchKmeans(data.frame(x = x), clusters = ncomp, batch_size = 20, num_init = 5, max_iters = 100, 
                        init_fraction = 0.2, initializer = 'kmeans++', early_stop_iter = 10,
                        verbose = F)
    cen = sort(as.vector(fit$centroids))
    cutoff = (cen[-1] + cen[1:(length(cen)-1)])/2
    a = integer(length(x))
    a[x < cutoff[1]] = 1
    if (length(cutoff) >= 2) {
      for (i in 2:length(cutoff)) {
        a[x >= cutoff[i-1] & x < cutoff[i]] = i
      }
    }
    a[x >= cutoff[length(cutoff)]] = ncomp
    
  } else if (init.method == "k-means") {
    a <- kmeans(x, centers = ncomp)$cluster
  }
  else {
    a <- cutree(hclust(dist(x)), ncomp)
  }
  res <- list()
  for (i in 1:ncomp) {
    res[[i]] <- x[a == i]
  }
  count <- sapply(res, length)
  pi <- count/sum(count)
  mu <- sapply(res, mean)
  sd <- sapply(res, sd)
  order <- order(mu)
  pi <- pi[order]
  mu <- mu[order]
  sd <- sd[order]
  list(pi = pi, mu = mu, sd = sd)
}