## plot lnorm mixtures

plotlnorm <- function(x, detail, smoothness, breaks, xlim, ylim, lwd, lty, ...) {
	pi <- x$pi
	mu <- x$mu
	sd <- x$sd
	mulog <- x$mulog
	sdlog <- x$sdlog
	data <- x$data

	xlow <- max(min(mu - 3.5 * sd), 0)
	xupp <- max(mu + 4 * sd)
	xseq <- seq(xlow, xupp, length = smoothness)
	res <- matrix(NA, nrow = length(xseq), ncol = length(pi))
	for(i in 1:length(pi)) {
		res[ ,i] <-  pi[i] * dlnorm(xseq, mulog[i], sdlog[i])
	}
	yt <- apply(res, 1, sum)

	# plot parameters checking
	if(missing(breaks)) {
		breaks <- 30
	}
	if(missing(xlim)) {
		xlim <- c(xlow, xupp)
	}
	if(missing(ylim)) {
		if(is.matrix(data)) {
			count <- data[, 3]
			max_freq <- max(count) / (sum(count) * max(diff(data[, 1]))) * 1.2
			ylim <- c(0, max(c(yt, max_freq)))
		} else {
			brks <- seq(min(data), max(data), length = breaks + 1)
			tmp <- bin(data, brks = brks)
			count <- tmp[, 3]
			max_freq <- max(count) / (sum(count) * (brks[2] - brks[1]))
			ylim <- c(0, max(c(yt, max_freq)))
		}
	}
	if(is.matrix(data)) {
		breaks <- sort(unique(c(data[, 1], data[, 2])))
		data <- reinstate(data)
	}
	if(missing(lwd)) {
		lwd <- 2
	}
	if(missing(lty)) {
		lty <- 1
	}
	# plot
	hist(data, freq = FALSE, breaks = breaks, xlim = xlim, ylim = ylim, ...)

	if(detail) {
		for(i in 1:length(pi)) {
			lines(xseq, res[, i], col = i + 1, lwd = 1.5)
		}
	}
	lines(xseq, yt, lty = lty, lwd = lwd, col = "black")
}
