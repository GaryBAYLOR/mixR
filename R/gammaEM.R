## gamma mixture model

gammaEM <- function(x, ncomp = NULL, pi = NULL, mu = NULL, sd = NULL, ev, 
                    mstep.method, init.method, tol = 1e-6, max_iter = 500) {	
	# check if initial values are provided
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
  if(max_iter < 200) stop("'max_iter' is too small! Recommend to leave it as default or set it at least 200 to reduce the risk of EM algorithm not converging.")
  
	alpha <- to_shape_rate_gamma(mu, sd)[[1]]
	lambda <- to_shape_rate_gamma(mu, sd)[[2]]
	
	n <- length(x)
	ncomp <- length(pi)
	
	fit <- gamma_C(x, pi, alpha, lambda, method = mstep.method, max_iter, tol)
	
	pi <- fit[[1]]
	mu <- fit[[2]]
	sd <- fit[[3]]
	alpha <- fit[[4]]
	lambda <- fit[[5]]
	loglik <- fit[[6]]
	iter <- fit[[7]]
	comp.prob <- fit[[8]]
	
	aic <- (-2) * loglik + 2 * (3 * ncomp - 1)
	bic <- (-2) * loglik + log(n) * (3 * ncomp - 1)
	res <- list(pi = pi, mu = mu, sd = sd, alpha = alpha, lambda = lambda, loglik = loglik, 
	            iter = iter, aic = aic, bic = bic, data = x, comp.prob = comp.prob, family = "gamma")
    class(res) <- "mixfitEM"
    res
}
