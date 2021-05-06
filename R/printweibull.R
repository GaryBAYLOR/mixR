## print weibull mixture

printweibull <- function(x, digits) {
	ncomp <- length(x$pi)
	name <- paste("comp", 1:ncomp, sep = "")
	res1 <- matrix(0, nrow = 5, ncol = ncomp)
	rownames(res1) <- c("pi", "mu", "sd", "shape", "scale")
	colnames(res1) <- name
	res1[1, ] <- x$pi
	res1[2, ] <- x$mu
	res1[3, ] <- x$sd
	res1[4, ] <- x$k
	res1[5, ] <- x$lambda
	res2 <- c(x$iter, x$aic, x$bic)
	names(res2) <- c("EM iteration", "AIC", "BIC")
	cat(paste("Weibull mixture model with", ncomp, "components\n"))
	print(round(res1, digits))
	cat("\n")
	cat(paste(c("EM iterations:", x$iter, "AIC:", round(x$aic, 2), 
	"BIC:", round(x$bic, 2), "log-likelihood:", round(x$loglik, 2))))
	cat('\n')
}