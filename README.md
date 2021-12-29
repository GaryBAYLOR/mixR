<!-- badges: start -->
[![CRAN Status Badge](http://www.r-pkg.org/badges/version/mixR)](https://CRAN.R-project.org/package=mixR)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/mixR)](https://cran.rstudio.com/web/packages/mixR/index.html)
[![CRAN Monthly Downloads](https://cranlogs.r-pkg.org/badges/mixR)](https://cran.r-project.org/package=mixR)

<!-- badges: end -->

# mixR: An R package for finite mixture modeling for both raw and binned data

## Installation

For stable/pre-compiled(for Windows and OS X) version, please install from [CRAN](https://CRAN.R-project.org/package=mixR):

```r
install.packages('mixR')
```

## Examples

* Fitting a normal mixture model
```{r}
library(mixR)

# generate data from a Normal mixture model
set.seed(102)
x1 = rmixnormal(1000, c(0.3, 0.7), c(-2, 3), c(2, 1))

# fit a Normal mixture model
mod1 = mixfit(x1, ncomp = 2)

# plot the fitted model
plot(mod1)

# fit a Normal mixture model (equal variance)
mod1_ev = mixfit(x1, ncomp = 2, ev = TRUE)
```

* Fitting a Weibull mixture model
```{r}
# generate data from a Weibull mixture model
x2 = rmixweibull(1000, c(0.4, 0.6), c(0.6, 1.3), c(0.1, 0.1))
mod2_weibull = mixfit(x2, family = 'weibull', ncomp = 2)
```
* Fitting a mixture model with binned data
```{r}
x1_binned = bin(x1, seq(min(x1), max(x1), length = 30))
mod1_binned = mixfit(x1_binned, ncomp = 2)
```

* Mixture model selection by BIC
```{r}
# Selecting the best g for Normal mixture model
s_normal = select(x2, ncomp = 2:6)

# Selecting the best g for Weibull mixture model
s_weibull = select(x2, ncomp = 2:6, family = 'weibull')

plot(s_weibull)
plot(s_normal)
```

* Mixture model selection by bootstrap likelihood ratio test (LRT)
```{r}
b1 = bs.test(x1, ncomp = c(2, 3))
plot(b1, main = 'Bootstrap LRT for Normal Mixture Models (g = 2 vs g = 3)')
b1$pvalue

b2 = bs.test(x2, ncomp = c(2, 4))
plot(b2, main = 'Bootstrap LRT for Normal Mixture Models (g = 2 vs g = 4)')
b2$pvalue
```

For more examples please check the vignette [An Introduction to mixR](https://cran.r-project.org/web/packages/mixR/vignettes/intro-to-mixr.pdf).
