check_inputs_for_rmix <- function(n, pi, mu, sd) {
  if(missing(n) || missing(pi) || missing(mu) || missing(sd)) {
    stop("'n', 'pi', 'mu' and 'sd' must be all provided!")
  } else if (!(is.vector(n, mode = 'numeric') && 
               is.vector(pi, mode = 'numeric') &&
               is.vector(mu,mode = 'numeric') &&
               is.vector(sd, mode = 'numeric'))) {
    stop("'n', 'pi', 'mu', 'sd' must be numeric!")
  } else if (length(n) != 1) {
    stop("'n' must be a scalar!")
  } else if (length(pi) != length(mu) || (length(pi) != length(sd))) {
    stop("'pi', 'mu' and 'sd' should all have the same length!")
  } else if (any(is.na(c(n, pi, mu, sd)))) {
    stop("'n', 'pi', 'mu' and 'sd' can not contain missing values!")
  } else if (any(c(n <= 1, pi <= 0, sd <= 0))) {
    stop("'n', 'pi' and 'sd' should all be positive and 'n' should be at least 1!")
  } else if (any(c(n, pi, mu, sd) > 1e7)) {
    stop("'n', 'pi', 'mu' and 'sd' contains data out of range (1e7)!")
  } else if (any(mu< -1e6)) {
    stop("'mu' is out of range!")
  }
}