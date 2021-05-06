densitynormal <- function(x, at, smoothness, cut) {
  pi <- x$pi
  mu <- x$mu
  sd <- x$sd
  if(missing(at)) {
    from <- min(mu - cut * sd)
    to <- max(mu + cut * sd)
    at <- seq(from, to, length = smoothness)
  }
  res <- matrix(NA, nrow = length(at), ncol = length(pi))
  for(i in 1:length(pi)) {
    res[ ,i] <-  pi[i] * dnorm(at, mu[i], sd[i])
  }
  colnames(res) = paste0('Comp', 1:length(pi))
  y <- apply(res, 1, sum)
  structure(list(x = at, y = y, comp = res), class = "densityEM")
}
