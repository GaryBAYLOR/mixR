densityweibull <- function(obj, at, smoothness, cut) {
  pi <- obj$pi
  mu <- obj$mu
  sd <- obj$sd
  k <- obj$k
  lambda <- obj$lambda
  if(missing(at)) {
    from <- max(min(mu - cut * sd), 0)
    to <- max(mu + 1.1 * cut * sd)
    at <- seq(from, to, length = smoothness)
  }
  res <- matrix(NA, nrow = length(at), ncol = length(pi))
  for(i in 1:length(pi)) {
    res[ ,i] <-  pi[i] * dweibull(at, k[i], lambda[i])
  }
  y <- apply(res, 1, sum)
  structure(list(x = at, y = y, comp = res), class = "densityEM")
}
