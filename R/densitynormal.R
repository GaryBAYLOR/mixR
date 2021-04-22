densitynormal <- function(x, smoothness, from, to, cut) {
	pi <- x$pi
	mu <- x$mu
	sd <- x$sd
	if(missing(from)) {
		from <- min(mu - cut * sd)
	}
	if(missing(to)) {
		to <- max(mu + cut * sd)
	}
	xseq <- seq(from, to, length = smoothness)
    res <- matrix(NA, nrow = length(xseq), ncol = length(pi))
	for(i in 1:length(pi)) {
		res[ ,i] <-  pi[i] * dnorm(xseq, mu[i], sd[i])
	}
	yt <- apply(res, 1, sum)
    structure(list(x = xseq, y = yt), class = "densityEM")
}

