## print lnorm mixture

printlnorm <- function(x, digits) {
	ncomp <- length(x$pi)
	name <- paste("comp", 1:ncomp, sep = "")
	res1 <- matrix(0, nrow = 5, ncol = ncomp)
	rownames(res1) <- c("pi", "mu", "sd", "mulog", "sdlog")
	colnames(res1) <- name
	res1[1, ] <- x$pi
	res1[2, ] <- x$mu
	res1[3, ] <- x$sd
	res1[4, ] <- x$mulog
	res1[5, ] <- x$sdlog
	res2 <- c(x$iter, x$aic, x$bic)
	names(res2) <- c("EM iteration", "AIC", "BIC")
	cat(paste("Lognormal mixture model with", ncomp, "components\n"))
	print(round(res1, digits))
	cat("\n")
	cat(paste(c("EM iterations:", x$iter, "AIC:", round(x$aic, digits), 
	"BIC:", round(x$bic, digits), "log-likelihood:", round(x$loglik, digits))))
}