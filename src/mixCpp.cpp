#include <Rcpp.h>
#include <iostream>
using namespace std;
using namespace Rcpp;

/////////////// BASIC FUNCTION DEFINITION ////////////////
// 1. seqC
NumericVector seqC(double from, double to, double by = 1) {
    int n = (to - from) / by + 1;
    NumericVector res(n);
    res(0) = from;
    for(int i = 1; i < n; i++) {
        res(i) = res(i - 1) + by;
    }
    return res;
}

// 2. cat: concatenate two doubles to a numeric vector
NumericVector cat(double x, double y) {
    NumericVector res(2);
    res(0) = x;
    res(1) = y;
    return res;
}


// [[Rcpp::export]]
NumericVector rcmean(NumericMatrix x, int margin) {
    if(margin == 1) {
        int nx = x.nrow();
        NumericVector res(nx);
        for(int i = 0; i < nx; i++) {
            res(i) = mean(x(i, _));
        }
        return res;
    } else {
        int ny = x.ncol();
        NumericVector res(ny);
        for(int j = 0; j < ny; j++) {
            res(j) = mean(x(_, j));
        }
        return res;
    }
}

// # 2. calculate row mean and column mean
// [[Rcpp::export]]
NumericVector rcsum(NumericMatrix x, int margin) {
    if(margin == 1) {
        int nx = x.nrow();
        NumericVector res(nx);
        for(int i = 0; i < nx; i++) {
            res(i) = sum(x(i, _));
        }
        return res;
    } else {
        int ny = x.ncol();
        NumericVector res(ny);
        for(int j = 0; j < ny; j++) {
            res(j) = sum(x(_, j));
        }
        return res;
    }
}

///////////////////////////////////////////////////////////

// # 3. expected value of z
// [[Rcpp::export]]
NumericMatrix expZ_norm_C(NumericVector x, NumericVector pi, NumericVector mu, NumericVector sd) {
	int n = x.size();
	pi = pi / sum(pi);
	int ncomp = pi.size();

	NumericMatrix res(n, ncomp);

	for(int j = 0; j < ncomp; j++) {
		res(_, j) = pi(j) * dnorm(x, mu(j), sd(j));
	}

    for(int i = 0; i < n; i++) {
    	res(i, _) = res(i, _) / sum(res(i, _));
    }
    return res;
}

// 4. compute the log likelihood
// [[Rcpp::export]]
double loglik_norm_C(NumericVector x, NumericVector pi, NumericVector mu, NumericVector sd) {
	int n = x.size();
	int ncomp = pi.size();
	NumericMatrix res(n, ncomp);
	NumericVector res2(n);

	for(int j = 0; j < ncomp; j++) {
		res(_, j) = pi(j) * dnorm(x, mu(j), sd(j));
	}
	return sum(log(rcsum(res, 1)));
}

// 5. Em algorithm for normal mixtures with unequal variances
// [[Rcpp::export]]
List norm_uv(NumericVector x, NumericVector pi, NumericVector mu, NumericVector sd,
            int max_iter = 500, double tol = 1e-6) {
    int n = x.size();
    int ncomp = mu.size();
    NumericVector pi_new(ncomp), mu_new(ncomp), var_new(ncomp), sd_new(ncomp);
    NumericMatrix Z(n, ncomp);
    int iter = 1;
    List res(6);
    while(iter < max_iter) {
    	Z = expZ_norm_C(x, pi, mu, sd);
    	pi_new = rcmean(Z, 2);

    	for(int j = 0; j < ncomp; j++) {
    		mu_new(j) = sum(x * Z(_, j)) / sum(Z(_, j));
    		var_new(j) = sum(Z(_, j) * pow(x - mu_new(j), 2)) / sum(Z(_, j));
    	}
    	sd_new = sqrt(var_new);

    	double diff = loglik_norm_C(x, pi_new, mu_new, sd_new) - loglik_norm_C(x, pi, mu, sd);
    	if((abs(diff) < tol) | (iter > max_iter)) break;
    	//pi = clone(pi_new);
    	//mu = clone(mu_new);
    	//sd = clone(sd_new);
        for(int j = 0; j < ncomp; j++) {
            pi(j) = pi_new(j);
            mu(j) = mu_new(j);
            sd(j) = sd_new(j);
        }
    	iter = iter + 1;
    }
    res(0) = pi_new;
    res(1) = mu_new;
    res(2) = sd_new;
    res(3) = loglik_norm_C(x, pi_new, mu_new, sd_new);
    res(4) = iter;
    res(5) = expZ_norm_C(x, pi_new, mu_new, sd_new);
    return res;
}

// 6. Em algorithm for normal mixtures with equal variance
// [[Rcpp::export]]
List norm_ev(NumericVector x, NumericVector pi, NumericVector mu, NumericVector sd,
            int max_iter = 500, double tol = 1e-6) {
    int n = x.size();
    int ncomp = mu.size();
    NumericVector pi_new(ncomp), mu_new(ncomp), var_new(ncomp), sd_new(ncomp);
    NumericMatrix Z(n, ncomp);
    int iter = 1;
    List res(6);
    while(iter < max_iter) {
    	Z = expZ_norm_C(x, pi, mu, sd);
    	pi_new = rcmean(Z, 2);

    	for(int j = 0; j < ncomp; j++) {
    		mu_new(j) = sum(x * Z(_, j)) / sum(Z(_, j));
    		var_new(j) = sum(Z(_, j) * pow(x - mu_new(j), 2));
    	}
    	double tmp = sum(var_new) / sum(Z);
    	tmp = sqrt(tmp);
    	for(int j = 0; j < ncomp; j++) {
    		sd_new(j) = tmp;
    	}

    	double diff = loglik_norm_C(x, pi_new, mu_new, sd_new) - loglik_norm_C(x, pi, mu, sd);
    	if((abs(diff) < tol) | (iter > max_iter)) break;
        //pi = clone(pi_new);
        //mu = clone(mu_new);
        //sd = clone(sd_new);
        for(int j = 0; j < ncomp; j++) {
            pi(j) = pi_new(j);
            mu(j) = mu_new(j);
            sd(j) = sd_new(j);
        }
    	iter = iter + 1;
    }
    res(0) = pi_new;
    res(1) = mu_new;
    res(2) = sd_new;
    res(3) = loglik_norm_C(x, pi_new, mu_new, sd_new);
    res(4) = iter;
    res(5) = expZ_norm_C(x, pi_new, mu_new, sd_new);
    return res;
}


/*
// 7. Simpson numerical integration for f with TWO arguments
// [[Rcpp::export]]
double intC2(Function f, double a, double b, double arg1, double arg2, int n = 100) {
	double h = (b - a) / n;
	double sum1 = as<double>(f(a + h/2, arg1, arg2));
	double sum2 = 0;

	for(int i = 1; i < n; i++) {
		sum1 += as<double>(f(a + h * i + h/2, arg1, arg2));
		sum2 += as<double>(f(a + h * i, arg1, arg2));
	}
	double f_a = as<double>(f(a, arg1, arg2));
	double f_b = as<double>(f(b, arg1, arg2));

	return (h/6) * (f_a + f_b + 4 * sum1 + 2 * sum2);
}

// 8. Simpson numerical integration for f with ONE arguments
// [[Rcpp::export]]
double intC1(Function f, double a, double b, double arg1, int n = 100) {
	double h = (b - a) / n;
	double sum1 = as<double>(f(a + h/2, arg1));
	double sum2 = 0;

	for(int i = 1; i < n; i++) {
		sum1 += as<double>(f(a + h * i + h/2, arg1));
		sum2 += as<double>(f(a + h * i, arg1));
	}
	double f_a = as<double>(f(a, arg1));
	double f_b = as<double>(f(b, arg1));

	return (h/6) * (f_a + f_b + 4 * sum1 + 2 * sum2);
}



// [[Rcpp::export]]
double intC1x(Function f, double a, double b, double arg1, int n = 100) {
    double h = (b - a) / n;
    NumericVector s1 = seqC(0, n-1) * h + h / 2 + a;
	NumericVector s2 = seqC(1, n-1) * h + a;
	NumericVector s3 = cat(a, b);

	double sum1 = sum(as<NumericVector>(f(s1, arg1)));
	double sum2 = sum(as<NumericVector>(f(s2, arg1)));
	double sum3 = sum(as<NumericVector>(f(s3, arg1)));

	return (4 * sum1 + 2 * sum2 + sum3) * h / 6.0;
}

// [[Rcpp::export]]
double intC2x(Function f, double a, double b, double arg1, double arg2, int n = 100) {
    double h = (b - a) / n;
    NumericVector s1 = seqC(0, n-1) * h + h / 2.0 + a;
	NumericVector s2 = seqC(1, n-1) * h + a;
	NumericVector s3 = cat(a, b);

	double sum1 = sum(as<NumericVector>(f(s1, arg1, arg2)));
	double sum2 = sum(as<NumericVector>(f(s2, arg1, arg2)));
	double sum3 = sum(as<NumericVector>(f(s3, arg1, arg2)));

	return (4 * sum1 + 2 * sum2 + sum3) * h / 6.0;
}

// [[Rcpp::export]]
NumericVector xdnorm(NumericVector x, double mu, double sd) {
	return x * dnorm(x, mu, sd);
}



// [[Rcpp::export]]
double xenorm(std::function<NumericVector(NumericVector, double, double)> f,
              double a, double b, double arg1, double arg2, int n = 100) {
    double h = (b - a) / n;
    NumericVector s1 = seqC(0, n-1) * h + h / 2.0 + a;
	NumericVector s2 = seqC(1, n-1) * h + a;
	NumericVector s3 = cat(a, b);

	double sum1 = sum(f(s1, arg1, arg2));
	double sum2 = sum(f(s2, arg1, arg2));
	double sum3 = sum(f(s3, arg1, arg2));

	return (4 * sum1 + 2 * sum2 + sum3) * h / 6.0;
}

*/


// density of truncted normal distribution
double dnorm_trunc(double x, double a, double b, double mu = 0, double sd = 1){
    double auc = R::pnorm(b, mu, sd, 1, 0) - R::pnorm(a, mu, sd, 1, 0);
    if(auc < 1e-10) {
        return 1.0/(b - a);
    } else {
        return R::dnorm(x, mu, sd, 0) / auc;
    }
}

// a function in the form of xf(x), in which f(x) is density of a normal distribution
double xdnorm(double x, double mu, double sd) {
    return x * R::dnorm(x, mu, sd, 0);
}

// integrate function used in C++ (for functions with 2 params)
double integrate(double(*f)(double, double, double), double a, double b, double arg1, double arg2, int n = 100) {
    double h = (b - a) / n;
    double sum1 = f(a + h/2, arg1, arg2);
    double sum2 = 0;

    for(int i = 1; i < n; i++) {
        sum1 += f(a + h * i + h/2, arg1, arg2);
        sum2 += f(a + h * i, arg1, arg2);
    }
    double f_a = f(a, arg1, arg2);
    double f_b = f(b, arg1, arg2);

    return (h/6) * (f_a + f_b + 4 * sum1 + 2 * sum2);
}

// expected value of truncated normal
// [[Rcpp::export]]
double enorm_trunc(double a, double b, double mu = 0, double sd = 1){
    double auc = R::pnorm(b, mu, sd, 1, 0) - R::pnorm(a, mu, sd, 1, 0);
    if(auc < 1e-10) {
        return (a + b) / 2;
    } else {
        double num = integrate(xdnorm, a, b, mu, sd);
        return num / auc;
    }
}

//////////////////////////////////////////////////////////////
//
//    NORMAL MIXTURE FOR GROUPED DATA
//
//////////////////////////////////////////////////////////////

// 1. function to calculate exnorm
// [[Rcpp::export]]
NumericMatrix exnorm(NumericMatrix data, NumericVector mu, NumericVector sd) {
    int n = data.nrow();
    int ncomp = mu.size();
    NumericVector a = data(_, 0);
    NumericVector b = data(_, 1);
    NumericMatrix res(n, ncomp);
    for(int i = 0; i < n; i++) {
        for(int j = 0; j < ncomp; j++) {
            res(i, j) = enorm_trunc(a(i), b(i), mu(j), sd(j));
        }
    }
    return res;
}

// [[Rcpp::export]]
NumericMatrix txnorm(NumericVector pi, NumericVector mu, NumericVector sd, NumericMatrix ex) {
    int n = ex.nrow();
    int ncomp = mu.size();
    NumericMatrix res(n, ncomp);
    NumericMatrix A(n, ncomp);
    for(int j = 0; j < ncomp; j++) {
        A(_, j) = pi(j) * dnorm(ex(_, j), mu(j), sd(j));
    }
    for(int i = 0; i < n; i++){
        res(i, _) = A(i, _) / sum(A(i, _));
    }
    return res;
}

// loglikelihood for grouped data
// [[Rcpp::export]]
double loglik_norm_gC(NumericMatrix data, NumericVector pi, NumericVector mu, NumericVector sd) {
    int n = data.nrow();
    int ncomp = mu.size();
    NumericVector res(n);
    NumericMatrix low(n, ncomp), upp(n, ncomp);
    double tmp;
    for(int j = 0; j < ncomp; j++) {
        low(_, j) = pnorm(data(_, 0), mu(j), sd(j));
        upp(_, j) = pnorm(data(_, 1), mu(j), sd(j));
    }
    for(int i = 0; i < n; i++) {
        tmp = sum((upp(i, _) - low(i, _)) * pi);
        res(i) = data(i, 2) * log(tmp);
    }
    return sum(res);
}

// normal mixture for grouped data (unequal variances)
// [[Rcpp::export]]
List norm_uv_g(NumericMatrix data, NumericVector pi, NumericVector mu, NumericVector sd,
               int max_iter = 100, double tol = 1e-4) {
    int n = data.nrow();
    int ncomp = mu.size();
    NumericVector pi_new(ncomp), mu_new(ncomp), var_new(ncomp), sd_new(ncomp);
    NumericMatrix ex(n, ncomp), tx(n, ncomp);
    int iter = 1;
    List res(6);
    while(iter < max_iter) {
        ex = exnorm(data, mu, sd);
        tx = txnorm(pi, mu, sd, ex);

        for(int j = 0; j < ncomp; j++) {
            pi_new(j) = sum(tx(_, j) * data(_, 2)) / sum(data(_, 2));
            mu_new(j) = mean(data(_, 2) * tx(_, j) * ex(_, j)) / mean(data(_, 2) * tx(_, j));
            var_new(j) = mean(data(_, 2) * tx(_, j) * pow(ex(_, j) - mu_new(j), 2)) / mean(data(_, 2) * tx(_, j));
        }
        sd_new = sqrt(var_new);

        double diff = loglik_norm_gC(data, pi_new, mu_new, sd_new) - loglik_norm_gC(data, pi, mu, sd);
        if((abs(diff) < tol) | (iter > max_iter)) break;
        //pi = clone(pi_new); // use 'clone' is important
        //mu = clone(mu_new);
        //sd = clone(sd_new);
        for(int j = 0; j < ncomp; j++) {
            pi(j) = pi_new(j);
            mu(j) = mu_new(j);
            sd(j) = sd_new(j);
        }
        iter = iter + 1;
    }
    res(0) = pi_new;
    res(1) = mu_new;
    res(2) = sd_new;
    res(3) = loglik_norm_gC(data, pi_new, mu_new, sd_new);
    res(4) = iter;
    ex = exnorm(data, mu_new, sd_new);
    tx = txnorm(pi_new, mu_new, sd_new, ex);
    res(5) = tx;
    return res;
}

// normal mixture for grouped data (equal variances)
// [[Rcpp::export]]
List norm_ev_g(NumericMatrix data, NumericVector pi, NumericVector mu, NumericVector sd,
               int max_iter = 1000, double tol = 1e-4) {
    int n = data.nrow();
    int ncomp = mu.size();
    NumericVector pi_new(ncomp), mu_new(ncomp), var_new(ncomp), sd_new(ncomp), tmp1(ncomp), tmp2(ncomp);
    NumericMatrix ex(n, ncomp), tx(n, ncomp);
    int iter = 1;
    List res(6);
    while(iter < max_iter) {
        ex = exnorm(data, mu, sd);
        tx = txnorm(pi, mu, sd, ex);
        for(int j = 0; j < ncomp; j++) {
            pi_new(j) = sum(tx(_, j) * data(_, 2)) / sum(data(_, 2));
            mu_new(j) = mean(data(_, 2) * tx(_, j) * ex(_, j)) / mean(data(_, 2) * tx(_, j));
            tmp1(j) = sum(data(_, 2) * tx(_, j) * pow(ex(_, j) - mu_new(j), 2));
            tmp2(j) = sum(data(_, 2) * tx(_, j));
        }
        var_new = rep(sum(tmp1) / sum(tmp2), ncomp);
        sd_new = sqrt(var_new);

        double diff = loglik_norm_gC(data, pi_new, mu_new, sd_new) - loglik_norm_gC(data, pi, mu, sd);
        if((abs(diff) < tol) | (iter > max_iter)) break;
        // pi = clone(pi_new); // use 'clone' is important
        // mu = clone(mu_new);
        // sd = clone(sd_new);
        for(int j = 0; j < ncomp; j++) {
            pi(j) = pi_new(j);
            mu(j) = mu_new(j);
            sd(j) = sd_new(j);
        }
        iter = iter + 1;
    }
    res(0) = pi_new;
    res(1) = mu_new;
    res(2) = sd_new;
    res(3) = loglik_norm_gC(data, pi_new, mu_new, sd_new);
    res(4) = iter;
    ex = exnorm(data, mu_new, sd_new);
    tx = txnorm(pi_new, mu_new, sd_new, ex);
    res(5) = tx;
    return res;
}



//////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////

// # 3. expected value of z
// [[Rcpp::export]]
NumericMatrix expZ_gamma_C(NumericVector x, NumericVector pi, NumericVector alpha, NumericVector lambda) {
    int n = x.size();
    pi = pi / sum(pi);
    int ncomp = pi.size();

    NumericMatrix res(n, ncomp);

    for(int j = 0; j < ncomp; j++) {
        res(_, j) = pi(j) * dgamma(x, alpha(j), 1 / lambda(j));
    }

    for(int i = 0; i < n; i++) {
        res(i, _) = res(i, _) / sum(res(i, _));
    }
    return res;
}

// 4. compute the log likelihood
// [[Rcpp::export]]
double loglik_gamma_C(NumericVector x, NumericVector pi, NumericVector alpha, NumericVector lambda) {
    int n = x.size();
    int ncomp = pi.size();
    NumericMatrix res(n, ncomp);

    for(int j = 0; j < ncomp; j++) {
        res(_, j) = pi(j) * dgamma(x, alpha(j), 1 / lambda(j));

        // the second par of dgamma in Rcpp us scale !
    }
    return sum(log(rcsum(res, 1)));
}


// the function we want to find its root, for original data, n is a vector of one's
double g_gamma(double alpha, NumericVector x, NumericVector Zj, double delta) {
    return mean(Zj) * (log(alpha) - log(delta)) + mean(Zj * log(x)) -
    mean(Zj) * R::digamma(alpha);
}

double g_gamma(double alpha, NumericVector n, NumericVector ex, NumericVector tx, double delta) {
    return mean(n * tx) * (log(alpha) - log(delta)) + mean(n * tx * log(ex)) -
    mean(n * tx) * R::digamma(alpha);
}

double g_gamma_diff(double alpha, NumericVector n, NumericVector ex, NumericVector tx, double delta) {
    return mean(n * tx) * (1 / alpha - R::trigamma(alpha));
}

// The Newton algorithm to get MLE
NumericVector newton_gamma_C(NumericVector n, NumericVector ex, NumericVector tx, double alpha = 1, int max_iter = 100) {
    int iter = 1;
    double alpha_new, lambda;
    double delta = mean(n * tx * ex) / mean(n * tx);

    while(iter < max_iter) {
        alpha_new = alpha - g_gamma(alpha, n, ex, tx, delta) / g_gamma_diff(alpha, n, ex, tx, delta);
        if(abs(alpha_new - alpha) < 1e-4) break;
        alpha = alpha_new;
    }
    lambda = alpha_new / delta;

    NumericVector res(2);
    res(0) = alpha_new;
    res(1) = lambda;
    return res;
}

// Bisection method to get MLE
// [[Rcpp::export]]
NumericVector gamma_bisection_C(NumericVector n, NumericVector ex, NumericVector tx, double tol = 1e-4,
                                int max_iter = 100, double xleft = 0.1, double xright = 5) {
    int iter = 1;
    double delta = mean(n * tx * ex) / mean(n * tx);

    while(g_gamma(xleft, n, ex, tx, delta) * g_gamma(xright, n, ex, tx, delta) > 0) {
        xright = xright * 2;
        xleft = xleft / 2;
    }

    double temp, ftemp, fright, alpha, lambda;
    double xfinal = (xleft + xright) / 2;

    while(iter < max_iter) {
        temp = (xleft + xright) / 2;
        ftemp = g_gamma(temp, n, ex, tx, delta);
        fright = g_gamma(xright, n, ex, tx, delta);

        if(abs(ftemp) < tol) {
            xfinal = temp;
            break;
        } else {
            if(R::sign(ftemp) == R::sign(fright)) {
                xright = temp;
            } else {
                xleft = temp;
            }
        }
        iter += 1;
    }

    alpha = xfinal;
    lambda = alpha / delta;
    NumericVector res(2);
    res(0) = alpha;
    res(1) = lambda;
    return res;
}


// 5. parameter transformation
// [[Rcpp::export]]
List to_mu_sd_gamma_C(NumericVector alpha, NumericVector lambda) {
    NumericVector mu = alpha / lambda;
    NumericVector sd = sqrt(alpha) / lambda;
    List res(2);
    res(0) = mu;
    res(1) = sd;
    return res;
}


// [[Rcpp::export]]
List to_alpha_lambda_gamma_C(NumericVector mu, NumericVector sd) {
    NumericVector alpha = pow(mu / sd, 2);
    NumericVector lambda = mu / pow(sd, 2);
    List res(2);
    res(0) = alpha;
    res(1) = lambda;
    return res;
}


// 6. Em algorithm for gamma mixtures
// [[Rcpp::export]]
List gamma_C(NumericVector x, NumericVector pi, NumericVector alpha, NumericVector lambda, String method = "Newton",
             int max_iter = 500, double tol = 1e-6) {
    int n = x.size();
    int ncomp = pi.size();
    NumericVector pi_new(ncomp), alpha_new(ncomp), lambda_new(ncomp);
    NumericMatrix Z(n, ncomp), par(ncomp, 2);
    int iter = 1;
    List res(8);
    NumericVector nx(n);
    for(int j = 0; j < n; j++) {
        nx(j) = 1.0;
    }

    while(iter < max_iter) {
        Z = expZ_gamma_C(x, pi, alpha, lambda);

        pi_new = rcmean(Z, 2);
        for(int j = 0; j < ncomp; j++) {
            if(method == "Newton") {
                par(j,_) = newton_gamma_C(nx, x, Z(_,j));
            } else {
                par(j,_) = gamma_bisection_C(nx, x, Z(_,j));
            }
        }
        alpha_new = par(_, 0);
        lambda_new = par(_, 1);

        double diff = loglik_gamma_C(x, pi_new, alpha_new, lambda_new) - loglik_gamma_C(x, pi, alpha, lambda);
        // cout << loglik_gamma_C(x, pi_new, alpha_new, lambda_new) << " " << iter << endl;
        if(abs(diff) < tol) break;
        for(int j = 0; j < ncomp; j++) {
            pi(j) = pi_new(j);
            alpha(j) = alpha_new(j);
            lambda(j) = lambda_new(j);
        }
        iter = iter + 1;
    }

    List pars = to_mu_sd_gamma_C(alpha_new, lambda_new);
    NumericVector mu = pars(0);
    NumericVector sd = pars(1);
    res(0) = pi_new;
    res(1) = mu;
    res(2) = sd;
    res(3) = alpha_new;
    res(4) = lambda_new;
    res(5) = loglik_gamma_C(x, pi_new, alpha_new, lambda_new);
    res(6) = iter;
    res(7) = expZ_gamma_C(x, pi_new, alpha_new, lambda_new);
    return res;
}

///////////////////////////////////////////////////////////
//
//   GAMMA MIXTURE FOR GROUPED DATA
//
///////////////////////////////////////////////////////////

// a function in the form of xf(x), in which f(x) is density of a gamma distribution
double xdgamma(double x, double alpha, double lambda) {
    return x * R::dgamma(x, alpha, 1/lambda, 0);
}

// expected value of truncated weibull
// [[Rcpp::export]]
double egamma_trunc(double a, double b, double alpha, double lambda){
    double auc = R::pgamma(b, alpha, 1/lambda, 1, 0) - R::pgamma(a, alpha, 1/lambda, 1, 0);
    if(auc < 1e-10) {
        return (a + b) / 2;
    } else {
        double num = integrate(xdgamma, a, b, alpha, lambda);
        return num / auc;
    }
}

// 1. expected value of X
// [[Rcpp::export]]
NumericMatrix EXgamma_C(NumericMatrix data, NumericVector alpha, NumericVector lambda) {
    int n = data.nrow();
    int ncomp = alpha.size();
    NumericVector a = data(_, 0);
    NumericVector b = data(_, 1);
    NumericMatrix res(n, ncomp);
    for(int i = 0; i < n; i++) {
        for(int j = 0; j < ncomp; j++) {
            res(i, j) = egamma_trunc(a(i), b(i), alpha(j), lambda(j));
        }
    }
    return res;
}

// [[Rcpp::export]]
NumericMatrix TXgamma_C(NumericVector pi, NumericVector alpha, NumericVector lambda, NumericMatrix ex) {
    int n = ex.nrow();
    int ncomp = alpha.size();
    NumericMatrix res(n, ncomp);
    NumericMatrix A(n, ncomp);
    for(int j = 0; j < ncomp; j++) {
        A(_, j) = pi(j) * dgamma(ex(_, j), alpha(j), 1 / lambda(j)); // !!! ONE OVER LAMBDA
    }
    for(int i = 0; i < n; i++){
        res(i, _) = A(i, _) / sum(A(i, _));
    }
    return res;
}

// loglikelihood for grouped data
// [[Rcpp::export]]
double loglik_gamma_g_C(NumericMatrix data, NumericVector pi, NumericVector alpha, NumericVector lambda) {
    int n = data.nrow();
    int ncomp = alpha.size();
    NumericVector res(n);
    NumericMatrix low(n, ncomp), upp(n, ncomp);
    double tmp;
    for(int j = 0; j < ncomp; j++) {
        low(_, j) = pgamma(data(_, 0), alpha(j), 1/lambda(j));
        upp(_, j) = pgamma(data(_, 1), alpha(j), 1/lambda(j));
    }
    for(int i = 0; i < n; i++) {
        tmp = sum((upp(i, _) - low(i, _)) * pi);
        res(i) = data(i, 2) * log(tmp);
    }
    return sum(res);
}


// weibull mixture models fitted to grouped data
// [[Rcpp::export]]
List gamma_g_C(NumericMatrix data, NumericVector pi, NumericVector alpha, NumericVector lambda, String method = "Newton", int max_iter = 500, double tol = 1e-6) {
    NumericVector count = data(_, 2);
    int n = data.nrow();
    int ncomp = alpha.size();
    NumericVector pi_new(ncomp), alpha_new(ncomp), lambda_new(ncomp);
    NumericMatrix ex(n, ncomp), tx(n, ncomp), par(ncomp, 2);
    int iter = 1;
    List res(8);

    while(iter < max_iter) {
        // e-step
        ex = EXgamma_C(data, alpha, lambda);
        tx = TXgamma_C(pi, alpha, lambda, ex);

        // m-step
        for(int j = 0; j < ncomp; j++) {
            pi_new(j) = sum(tx(_, j) * count) / sum(rcsum(tx, 1) * count);
            if(method == "Newton") {
                par(j,_) = newton_gamma_C(count, ex(_, j), tx(_,j));
            } else {
                par(j,_) = gamma_bisection_C(count, ex(_, j), tx(_,j));
            }
        }
        alpha_new = par(_, 0);
        lambda_new = par(_, 1);
        double diff = loglik_gamma_g_C(data, pi_new, alpha_new, lambda_new) - loglik_gamma_g_C(data, pi, alpha, lambda);

        if(abs(diff) < tol) break;
        for(int j = 0; j < ncomp; j++) {
            pi(j) = pi_new(j);
            alpha(j) = alpha_new(j);
            lambda(j) = lambda_new(j);
        }
        iter = iter + 1;
    }
    List pars = to_mu_sd_gamma_C(alpha_new, lambda_new);
    NumericVector mu = pars(0);
    NumericVector sd = pars(1);
    res(0) = pi_new;
    res(1) = mu;
    res(2) = sd;
    res(3) = alpha_new;
    res(4) = lambda_new;
    res(5) = loglik_gamma_g_C(data, pi_new, alpha_new, lambda_new);
    res(6) = iter;
    ex = EXgamma_C(data, alpha_new, lambda_new);
    tx = TXgamma_C(pi_new, alpha_new, lambda_new, ex);
    res(7) = tx;
    return res;
}



//////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////


// # 3. expected value of z
// [[Rcpp::export]]
NumericMatrix expZ_weib_C(NumericVector x, NumericVector pi, NumericVector k, NumericVector lambda) {
    int n = x.size();
    pi = pi / sum(pi);
    int ncomp = pi.size();

    NumericMatrix res(n, ncomp);

    for(int j = 0; j < ncomp; j++) {
        res(_, j) = pi(j) * dweibull(x,k(j), lambda(j));
    }

    for(int i = 0; i < n; i++) {
        res(i, _) = res(i, _) / sum(res(i, _));
    }
    return res;
}


// 4. compute the log likelihood
// [[Rcpp::export]]
double loglik_weib_C(NumericVector x, NumericVector pi, NumericVector k, NumericVector lambda) {
    int n = x.size();
    int ncomp = pi.size();
    NumericMatrix res(n, ncomp);

    for(int j = 0; j < ncomp; j++) {
        res(_, j) = pi(j) * dweibull(x, k(j), lambda(j));
    }
    return sum(log(rcsum(res, 1)));
}



// the function we want to find its root
// [[Rcpp::export]]
double g_weib(double r, NumericVector n, NumericVector ex, NumericVector tx) {
    // n should be in the form of a vector
    double A = sum(n * tx * pow(ex, r) * log(ex));
    double B = sum(n * tx * pow(ex, r));
    double C = sum(n * tx  * log(ex));
    double D = sum(n * tx);
    // cout << A << " " << B << " " << C << " " << D << " " << endl;
    return A / B - 1 / r - C / D;
}

// the derivative of g_weib
double g_weib_diff(double r, NumericVector n, NumericVector ex, NumericVector tx) {
    // n should be in the form of a vector
    double A = sum(n * tx * pow(ex, r) * pow(log(ex), 2));
    double B = sum(n * tx * pow(ex, r));
    double C = sum(n * tx * pow(ex, r) * log(ex));
    return A / B + 1 / pow(r, 2) - pow(C / B, 2);
}

// Newton method to get MLE
// [[Rcpp::export]]
NumericVector newton_weib_C(NumericVector n, NumericVector ex, NumericVector tx, double r = 1, int max_iter = 100) {
    int iter = 1;
    double theta, lambda, k, r_new;

    while(iter < max_iter) {
        r_new = r - g_weib(r, n, ex, tx) / g_weib_diff(r, n, ex, tx);
        if(abs(r_new - r) < 1e-4) break;
        r = r_new;
    }
    theta = sum(n * tx * pow(ex, r_new)) / sum(n * tx);
    k = r_new;
    lambda = pow(theta, 1.0 / k);

    NumericVector res(2);
    res(0) = k;
    res(1) = lambda;
    return res;
}

// Bisection method to get MLE
// [[Rcpp::export]]
NumericVector weib_bisection_C(NumericVector n, NumericVector ex, NumericVector tx, double tol = 1e-4,
                               int max_iter = 100, double xleft = 0.1, double xright = 5) {
    int iter = 1;

    while(g_weib(xleft, n, ex, tx) * g_weib(xright, n, ex, tx) > 0) {
        xright = xright * 2;
        xleft = xleft / 2;
    }

    double temp, ftemp, fright, k, theta, lambda;
    double xfinal = (xleft + xright) / 2;

    while(iter < max_iter) {
        temp = (xleft + xright) / 2;
        ftemp = g_weib(temp, n, ex, tx);
        fright = g_weib(xright, n, ex, tx);

        if(abs(ftemp) < tol) {
            xfinal = temp;
            break;
        } else {
            if(R::sign(ftemp) == R::sign(fright)) {
                xright = temp;
            } else {
                xleft = temp;
            }
        }
        iter += 1;
    }

    theta = sum(n * tx * pow(ex, xfinal)) / sum(n * tx);
    k = xfinal;
    lambda = pow(theta, 1 / k);

    NumericVector res(2);
    res(0) = k;
    res(1) = lambda;
    return res;
}


// 5. parameter transformation
// [[Rcpp::export]]
List to_mu_sd_weib_C(NumericVector k, NumericVector lambda) {
    int n = k.size();
    NumericVector mu(n), var(n), sd(n);
    for(int j=0; j<n; j++) {
        mu[j] = lambda[j] * R::gammafn(1 + 1/k[j]);
        var[j] = pow(lambda[j], 2) * (R::gammafn(1 + 2/k[j]) - pow(R::gammafn(1 + 1/k[j]), 2));
    }
    sd = sqrt(var);
    List res(2);
    res(0) = mu;
    res(1) = sd;
    return res;
}

// [[Rcpp::export]]
List to_k_lambda_weib_C(NumericVector mu, NumericVector sd) {
    int n = mu.size();
    NumericVector k(n), lambda(n);
    for(int j=0; j<n; j++) {
        k[j] = pow(sd[j] / mu[j], -1.086);
        lambda[j] = mu[j] / R::gammafn(1 + 1/k[j]);
    }
    List res(2);
    res(0) = k;
    res(1) = lambda;
    return res;
}


// 6. Em algorithm for weibull mixtures
// [[Rcpp::export]]
List weib_C(NumericVector x, NumericVector pi, NumericVector k, NumericVector lambda, String method = "Newton", int max_iter = 500, double tol = 1e-6) {
    int n = x.size();
    int ncomp = pi.size();
    NumericVector pi_new(ncomp), k_new(ncomp), lambda_new(ncomp), mod(2);
    NumericMatrix Z(n, ncomp);
    int iter = 1;
    List res(8);
    NumericVector nx(n);
    for(int j = 0; j < n; j++) {
        nx(j) = 1.0;
    }

    while(iter < max_iter) {
        Z = expZ_weib_C(x, pi, k, lambda);

        pi_new = rcmean(Z, 2);
        for(int j = 0; j < ncomp; j++) {
            if(method == "Newton") {
                mod = newton_weib_C(nx, x, Z(_,j));
            } else {
                mod = weib_bisection_C(nx, x, Z(_,j));
            }
            k_new(j) = mod(0);
            lambda_new(j) = mod(1);
        }

        double diff = loglik_weib_C(x, pi_new, k_new, lambda_new) - loglik_weib_C(x, pi, k, lambda);
        // cout << loglik_weib_C(x, pi_new, k_new, lambda_new) << " " << iter << endl;
        // cout << k_new(0) << " "<< k_new(1)  << " "<< lambda_new(0) << " " << lambda_new(1) << endl;
        if(abs(diff) < tol) break;
        for(int j = 0; j < ncomp; j++) {
            pi(j) = pi_new(j);
            k(j) = k_new(j);
            lambda(j) = lambda_new(j);
        }
        iter = iter + 1;
    }

    List pars = to_mu_sd_weib_C(k_new, lambda_new);
    NumericVector mu = pars(0);
    NumericVector sd = pars(1);
    res(0) = pi_new;
    res(1) = mu;
    res(2) = sd;
    res(3) = k_new;
    res(4) = lambda_new;
    res(5) = loglik_weib_C(x, pi_new, k_new, lambda_new);
    res(6) = iter;
    res(7) = expZ_weib_C(x, pi_new, k_new, lambda_new);
    return res;
}

/////////////////////////////////////////////////////////////
//
//  WEIBULL MIXTURE MODELS FOR GROUPED DATA
//
/////////////////////////////////////////////////////////////

// a function in the form of xf(x), in which f(x) is density of a weibull distribution
double xdweib(double x, double k, double lambda) {
    return x * R::dweibull(x, k, lambda, 0);
}

// expected value of truncated weibull
// [[Rcpp::export]]
double eweib_trunc(double a, double b, double k, double lambda){
    double auc = R::pweibull(b, k, lambda, 1, 0) - R::pweibull(a, k, lambda, 1, 0);
    if(auc < 1e-10) {
        return (a + b) / 2;
    } else {
        double num = integrate(xdweib, a, b, k, lambda);
        return num / auc;
    }
}

// 1. expected value of X
// [[Rcpp::export]]
NumericMatrix EXweib_C(NumericMatrix data, NumericVector k, NumericVector lambda) {
    int n = data.nrow();
    int ncomp = k.size();
    NumericVector a = data(_, 0);
    NumericVector b = data(_, 1);
    NumericMatrix res(n, ncomp);
    for(int i = 0; i < n; i++) {
        for(int j = 0; j < ncomp; j++) {
            res(i, j) = eweib_trunc(a(i), b(i), k(j), lambda(j));
        }
    }
    return res;
}

// [[Rcpp::export]]
NumericMatrix TXweib_C(NumericVector pi, NumericVector k, NumericVector lambda, NumericMatrix ex) {
    int n = ex.nrow();
    int ncomp = k.size();
    NumericMatrix res(n, ncomp);
    NumericMatrix A(n, ncomp);
    for(int j = 0; j < ncomp; j++) {
        A(_, j) = pi(j) * dweibull(ex(_, j), k(j), lambda(j));
    }
    for(int i = 0; i < n; i++){
        res(i, _) = A(i, _) / sum(A(i, _));
    }
    return res;
}

// loglikelihood for grouped data
// [[Rcpp::export]]
double loglik_weib_g_C(NumericMatrix data, NumericVector pi, NumericVector k, NumericVector lambda) {
    int n = data.nrow();
    int ncomp = k.size();
    NumericVector res(n);
    NumericMatrix low(n, ncomp), upp(n, ncomp);
    double tmp;
    for(int j = 0; j < ncomp; j++) {
        low(_, j) = pweibull(data(_, 0), k(j), lambda(j));
        upp(_, j) = pweibull(data(_, 1), k(j), lambda(j));
    }
    for(int i = 0; i < n; i++) {
        tmp = sum((upp(i, _) - low(i, _)) * pi);
        res(i) = data(i, 2) * log(tmp);
    }
    return sum(res);
}

// weibull mixture models fitted to grouped data
// [[Rcpp::export]]
List weib_g_C(NumericMatrix data, NumericVector pi, NumericVector k, NumericVector lambda, String method = "Newton", int max_iter = 500, double tol = 1e-6) {
    NumericVector count = data(_, 2);
    int n = data.nrow();
    int ncomp = k.size();
    NumericVector pi_new(ncomp), k_new(ncomp), lambda_new(ncomp);
    NumericMatrix ex(n, ncomp), tx(n, ncomp), par(ncomp, 2);
    int iter = 1;
    List res(8);

    while(iter < max_iter) {
        // e-step
        ex = EXweib_C(data, k, lambda);
        tx = TXweib_C(pi, k, lambda, ex);

        // m-step
        for(int j = 0; j < ncomp; j++) {
            pi_new(j) = sum(tx(_, j) * count) / sum(rcsum(tx, 1) * count);
            if(method == "Newton") {
                par(j,_) = newton_weib_C(count, ex(_, j), tx(_,j));
            } else {
                par(j,_) = weib_bisection_C(count, ex(_, j), tx(_,j));
            }
        }
        k_new = par(_, 0);
        lambda_new = par(_, 1);
        double diff = loglik_weib_g_C(data, pi_new, k_new, lambda_new) - loglik_weib_g_C(data, pi, k, lambda);

        if(abs(diff) < tol) break;
        for(int j = 0; j < ncomp; j++) {
            pi(j) = pi_new(j);
            k(j) = k_new(j);
            lambda(j) = lambda_new(j);
        }
        iter = iter + 1;
    }
    List pars = to_mu_sd_weib_C(k_new, lambda_new);
    NumericVector mu = pars(0);
    NumericVector sd = pars(1);
    res(0) = pi_new;
    res(1) = mu;
    res(2) = sd;
    res(3) = k_new;
    res(4) = lambda_new;
    res(5) = loglik_weib_g_C(data, pi_new, k_new, lambda_new);
    res(6) = iter;
    ex = EXweib_C(data, k_new, lambda_new);
    tx = TXweib_C(pi_new, k_new, lambda_new, ex);
    res(7) = tx;
    return res;
}

//////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////

// # 3. expected value of z
// [[Rcpp::export]]
NumericMatrix expZ_lnorm_C(NumericVector x, NumericVector pi, NumericVector mulog, NumericVector sdlog) {
    int n = x.size();
    pi = pi / sum(pi);
    int ncomp = pi.size();

    NumericMatrix res(n, ncomp);

    for(int j = 0; j < ncomp; j++) {
        res(_, j) = pi(j) * dlnorm(x, mulog(j), sdlog(j));
    }

    for(int i = 0; i < n; i++) {
        res(i, _) = res(i, _) / sum(res(i, _));
    }
    return res;
}

// 4. compute the log likelihood
// [[Rcpp::export]]
double loglik_lnorm_C(NumericVector x, NumericVector pi, NumericVector mulog, NumericVector sdlog) {
    int n = x.size();
    int ncomp = pi.size();
    NumericMatrix res(n, ncomp);

    for(int j = 0; j < ncomp; j++) {
        res(_, j) = pi(j) * dlnorm(x, mulog(j), sdlog(j));
    }
    return sum(log(rcsum(res, 1)));
}

// 5. parameter transformation
List to_mu_sd_C(NumericVector mulog, NumericVector sdlog) {
    NumericVector mu = exp(mulog + pow(sdlog, 2) / 2);
    NumericVector var = (exp(pow(sdlog, 2)) - 1) * exp(2 * mulog + pow(sdlog, 2));
    NumericVector sd = sqrt(var);
    List res(2);
    res(0) = mu;
    res(1) = sd;
    return res;
}

List to_mulog_sdlog_C(NumericVector mu, NumericVector sd) {
    NumericVector tmp = log(pow(sd / mu, 2) + 1);
    NumericVector mulog = log(mu) - 0.5 * tmp;
    NumericVector sdlog = sqrt(tmp);
    List res(2);
    res(0) = mulog;
    res(1) = sdlog;
    return res;
}


// 6. Em algorithm for LOG-normal mixtures
// [[Rcpp::export]]
List lnorm_C(NumericVector x, NumericVector pi, NumericVector mulog, NumericVector sdlog,
             int max_iter = 500, double tol = 1e-6) {
    int n = x.size();
    int ncomp = mulog.size();
    NumericVector pi_new(ncomp), mulog_new(ncomp), varlog_new(ncomp), sdlog_new(ncomp);
    NumericMatrix Z(n, ncomp);
    int iter = 1;
    List res(8);

    while(iter < max_iter) {
        Z = expZ_lnorm_C(x, pi, mulog, sdlog);

        pi_new = rcmean(Z, 2);
        for(int j = 0; j < ncomp; j++) {
            mulog_new(j) = sum(Z(_, j) * log(x)) / sum(Z(_, j));
            varlog_new(j) = sum(Z(_, j) * pow(log(x) - mulog_new(j), 2)) / sum(Z(_, j));
        }
        sdlog_new = sqrt(varlog_new);

        double diff = loglik_lnorm_C(x, pi_new, mulog_new, sdlog_new) - loglik_lnorm_C(x, pi, mulog, sdlog);
        if(abs(diff) < tol) break;
        for(int j = 0; j < ncomp; j++) {
            pi(j) = pi_new(j);
            mulog(j) = mulog_new(j);
            sdlog(j) = sdlog_new(j);
        }
        iter = iter + 1;
    }
    List pars = to_mu_sd_C(mulog, sdlog);
    NumericVector mu = pars(0);
    NumericVector sd = pars(1);
    res(0) = pi_new;
    res(1) = mu;
    res(2) = sd;
    res(3) = mulog_new;
    res(4) = sdlog_new;
    res(5) = loglik_lnorm_C(x, pi_new, mulog_new, sdlog_new);
    res(6) = iter;
    res(7) = expZ_lnorm_C(x, pi_new, mulog_new, sdlog_new);
    return res;
}

/////////////////////////////////////////////////////////////
//
//  LOG-NORMAL MIXTURE MODELS FOR GROUPED DATA
//
/////////////////////////////////////////////////////////////

// a function in the form of xf(x), in which f(x) is density of a lnorm distribution
double xdlnorm(double x, double mulog, double sdlog) {
    return x * R::dlnorm(x, mulog, sdlog, 0);
}

// expected value of truncated lnorm
// [[Rcpp::export]]
double elnorm_trunc(double a, double b, double mulog, double sdlog){
    double auc = R::plnorm(b, mulog, sdlog, 1, 0) - R::plnorm(a, mulog, sdlog, 1, 0);
    if(auc < 1e-10) {
        return (a + b) / 2;
    } else {
        double num = integrate(xdlnorm, a, b, mulog, sdlog);
        return num / auc;
    }
}

// 1. expected value of X
// [[Rcpp::export]]
NumericMatrix EXlnorm_C(NumericMatrix data, NumericVector mulog, NumericVector sdlog) {
    int n = data.nrow();
    int ncomp = mulog.size();
    NumericVector a = data(_, 0);
    NumericVector b = data(_, 1);
    NumericMatrix res(n, ncomp);
    for(int i = 0; i < n; i++) {
        for(int j = 0; j < ncomp; j++) {
            res(i, j) = elnorm_trunc(a(i), b(i), mulog(j), sdlog(j));
        }
    }
    return res;
}

// 2. expected value of Z
// [[Rcpp::export]]
NumericMatrix TXlnorm_C(NumericVector pi, NumericVector mulog, NumericVector sdlog, NumericMatrix ex) {
    int n = ex.nrow();
    int ncomp = mulog.size();
    NumericMatrix res(n, ncomp);
    NumericMatrix A(n, ncomp);
    for(int j = 0; j < ncomp; j++) {
        A(_, j) = pi(j) * dlnorm(ex(_, j), mulog(j), sdlog(j));
    }
    for(int i = 0; i < n; i++){
        res(i, _) = A(i, _) / sum(A(i, _));
    }
    return res;
}

// loglikelihood for grouped data for lnorm
// [[Rcpp::export]]
double loglik_lnorm_g_C(NumericMatrix data, NumericVector pi, NumericVector mulog, NumericVector sdlog) {
    int n = data.nrow();
    int ncomp = mulog.size();
    NumericVector res(n);
    NumericMatrix low(n, ncomp), upp(n, ncomp);
    double tmp;
    for(int j = 0; j < ncomp; j++) {
        low(_, j) = plnorm(data(_, 0), mulog(j), sdlog(j));
        upp(_, j) = plnorm(data(_, 1), mulog(j), sdlog(j));
    }
    for(int i = 0; i < n; i++) {
        tmp = sum((upp(i, _) - low(i, _)) * pi);
        res(i) = data(i, 2) * log(tmp);
    }
    return sum(res);
}

// 7. Em algorithm for LOG-normal mixtures (grouped data)
// [[Rcpp::export]]
List lnorm_g_C(NumericMatrix data, NumericVector pi, NumericVector mulog, NumericVector sdlog, int max_iter = 500, double tol = 1e-6) {

    NumericVector count = data(_, 2);
    int n = data.nrow();
    int ncomp = mulog.size();
    NumericVector pi_new(ncomp), mulog_new(ncomp), varlog_new(ncomp), sdlog_new(ncomp);
    NumericMatrix ex(n, ncomp), tx(n, ncomp),  par(ncomp, 2);
    int iter = 1;
    List res(8);

    while(iter < max_iter) {
        ex = EXlnorm_C(data, mulog, sdlog);
        tx = TXlnorm_C(pi, mulog, sdlog, ex);

        for(int j = 0; j < ncomp; j++) {
            pi_new(j) = sum(tx(_, j) * count) / sum(rcsum(tx, 1) * count);
            mulog_new(j) = sum(count * tx(_, j) * log(ex(_, j))) / sum(count * tx(_, j));
            varlog_new(j) = sum(count * tx(_, j) * pow(log(ex(_, j)) - mulog_new(j), 2)) / sum(count * tx(_, j));
        }
        sdlog_new = sqrt(varlog_new);

        double diff = loglik_lnorm_g_C(data, pi_new, mulog_new, sdlog_new) - loglik_lnorm_g_C(data, pi, mulog, sdlog);
        if(abs(diff) < tol) break;
        for(int j = 0; j < ncomp; j++) {
            pi(j) = pi_new(j);
            mulog(j) = mulog_new(j);
            sdlog(j) = sdlog_new(j);
        }
        iter = iter + 1;
    }
    List pars = to_mu_sd_C(mulog, sdlog);
    NumericVector mu = pars(0);
    NumericVector sd = pars(1);
    res(0) = pi_new;
    res(1) = mu;
    res(2) = sd;
    res(3) = mulog_new;
    res(4) = sdlog_new;
    res(5) = loglik_lnorm_g_C(data, pi_new, mulog_new, sdlog_new);
    res(6) = iter;
    ex = EXlnorm_C(data, mulog_new, sdlog_new);
    tx = TXlnorm_C(pi, mulog_new, sdlog_new, ex);
    res(7) =tx;
    return res;
}










