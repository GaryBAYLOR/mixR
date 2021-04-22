## plot normal mixture models (ggplot2)

ggplotnormal <- function(object, detail, smoothness, title = NULL, xlim, ylim,
                 xlab, ylab, breaks, ..., theme = c("grey", "bw"), trans = 0.5) {
	pi <- object$pi
	mu <- object$mu
	sd <- object$sd
	data <- object$data
	family <- object$family
	ncomp <- length(pi)

	if(missing(xlab)) {
		xlab <- "Data"
	}
	if(missing(ylab)) {
		ylab <- "Density"
	}
	if(missing(breaks)) {
		breaks <- 30
	}
	if(missing(xlim)) {
		xlim <- c(min(mu - 3.5 * sd), max(mu + 3.5 * sd))
	}

	# binwidth = (xlim[2] - xlim[1]) / breaks
	xseq <- seq(xlim[1], xlim[2], length = smoothness)
	res <- matrix(NA, nrow = length(xseq), ncol = length(pi))
	for(i in 1:length(pi)) {
		res[ ,i] <-  pi[i] * dnorm(xseq, mu[i], sd[i])
	}
	yt <- apply(res, 1, sum)

	if(missing(ylim)) {
		if(is.matrix(data)) {
			count <- data[, 3]
			max_freq <- max(count) / (sum(count) * max(diff(data[, 1])))
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
	} else {
		breaks <- brks
	}

	# prepare data frames
	df1 <- data.frame(x = rep(xseq, ncomp), comp = rep(1:ncomp, each = smoothness),
					  y = as.vector(res))
	df2 <- data.frame(x = xseq, y = yt)

	# plot
	if(is.null(title)) title = paste(family,"mixture density")
	if(detail) {
		add <- geom_polygon(data = df1, aes(x, y, fill = as.factor(comp)), alpha = trans)
	} else {
		add <- NULL
	}
	if(theme[1] == "bw") {
		theme <- theme_bw()
	} else {
		theme <- theme_grey()
	}
	ggplot(as.data.frame(data)) +
		geom_histogram(aes(x = data, y = ..density..),breaks = breaks, color = "black",
				   fill = "white", size = 0.3) + add + theme +
		geom_path(data = df2, aes(x, y), ...) +
	scale_fill_discrete(guide = guide_legend(title = "Comp")) +
	labs(title = title, x = xlab, y = ylab) +
	  theme(plot.title = element_text(hjust = 0.5))
}

