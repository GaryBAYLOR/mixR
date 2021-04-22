## normal mixture models (grouped data)

normalEM2 <- function(x, ncomp = NULL, pi = NULL, mu = NULL, sd = NULL, ev, 
                      mstep.method, init.method, tol = 1e-6, max_iter = 500) {
	# check if initial values are missing
  if(is.null(pi) & is.null(mu) & is.null(sd)) {
    if(is.null(ncomp)) stop("provide 'ncomp' or all of 'pi', 'mu' and 'sd'.")
    init <- initz(x, ncomp = ncomp, init.method)
    pi <- init$pi
    mu <- init$mu
    sd <- init$sd
    ncomp <- length(pi)
  } else if (is.null(pi) | is.null(mu) | is.null(sd)) {
    if(!is.null(pi)) ncomp <- length(pi)
    if(!is.null(mu)) ncomp <- length(mu)
    if(!is.null(sd)) ncomp <- length(sd)
    init <- initz(x, ncomp = ncomp, init.method)
    if(is.null(pi)) pi <- init$pi
    if(is.null(mu)) mu <- init$mu
    if(is.null(sd)) sd <- init$sd
  }
  if(length(pi) != length(mu) || length(pi) != length(sd) ) {
    stop("the length of 'pi', 'mu' and 'sd' should be the same.")
  }	
	count <- x[, 3]
	
	if(ev) {
		fit <- norm_ev_g(x, pi, mu, sd, max_iter, tol)
	} else {
		fit <- norm_uv_g(x, pi, mu, sd, max_iter, tol)
	}
    pi_new <- fit[[1]]
	mu_new <- fit[[2]]
	sd_new <- fit[[3]]
	loglik <- fit[[4]]
	iter <- fit[[5]]  
	comp.prob <- fit[[6]]
	
	aic <- -2* loglik + 2 * ifelse(ev, 2 * ncomp, 3 * ncomp - 1)
	bic <- -2* loglik + log(sum(count)) * ifelse(ev, 2 * ncomp, 3 * ncomp - 1)
	
	res <- list(pi = pi_new, mu = mu_new, sd = sd_new, iter = iter, loglik = loglik, 
			    aic = aic, bic = bic, data = x, comp.prob = comp.prob, family = "normal")
	class(res) <- "mixfitEM"
	return(res)
}
