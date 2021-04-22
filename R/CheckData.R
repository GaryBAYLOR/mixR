CheckData <- function(x, family) {
	if(!(is.matrix(x) && is.numeric(x) || is.numeric(x))) {
		stop("a numeric vector or matrix is required.", call. = FALSE)
	}
	if(is.matrix(x) && is.numeric(x)) {
		nc <- ncol(x)
		if(nc != 3) {
			stop("the matrix should has three columns.", call. = FALSE)
        }
        if(any(x[, 1] >= x[, 2])) {
			stop("the first column of input matrix should be less than the second column.", call. = FALSE) 
	    }
	    if(all(floor(x[, 3]) <= 0)) {
			stop("the third column of input matrix should be positive and not all less than one.", call. = FALSE)
		}
		if(nrow(x) < 10) stop("insufficient data for model estimation.", call. = FALSE)      
	} else {
		if(length(x) < 30) stop("insufficient data for model estimation.", call. = FALSE) 
	}
  if(family != "normal" & min(x) < 0) {
    stop("the data must be all positive.", call. = FALSE) 
  }
  if(sum(is.na(x) | is.nan(x) | !is.finite(x)) > 0) stop("Please remove NA/NaN/Inf in the input data.", call. = FALSE)
}
