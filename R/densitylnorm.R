densitylnorm <- function(x, at, smoothness, cut) {
  pi <- x$pi
  mu <- x$mu
  sd <- x$sd
  mulog <- x$mulog
  sdlog <- x$sdlog
  if(missing(at)) {
    from <- max(min(mu - cut * sd), 0)
    to <- max(mu + 1.1 * cut * sd)
    at <- seq(from, to, length = smoothness)
  }
  res <- matrix(NA, nrow = length(at), ncol = length(pi))
  for(i in 1:length(pi)) {
    res[ ,i] <-  pi[i] * dlnorm(at, mulog[i], sdlog[i])
  }
  y <- apply(res, 1, sum)
  structure(list(x = at, y = y, comp = res), class = "densityEM")
}