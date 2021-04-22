densitygamma <- function(x, smoothness, from, to, cut) {
	pi <- x$pi
	mu <- x$mu
	sd <- x$sd
	alpha <- x$alpha
	lambda <- x$lambda
	if(missing(from)) {
		from <- max(min(mu - cut * sd), 0)
	}
	if(missing(to)) {
		to <- max(mu + 1.2 * cut * sd)
	}
	xseq <- seq(from, to, length = smoothness)
    res <- matrix(NA, nrow = length(xseq), ncol = length(pi))
	for(i in 1:length(pi)) {
		res[ ,i] <-  pi[i] * dgamma(xseq, alpha[i], lambda[i])
	}
	yt <- apply(res, 1, sum)
    structure(list(x = xseq, y = yt), class = "densityEM")
}
