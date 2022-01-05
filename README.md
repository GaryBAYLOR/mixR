<!-- badges: start -->
[![CRAN Status Badge](http://www.r-pkg.org/badges/version/mixR)](https://CRAN.R-project.org/package=mixR)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/mixR)](https://cran.rstudio.com/web/packages/mixR/index.html)
[![CRAN Monthly Downloads](https://cranlogs.r-pkg.org/badges/mixR)](https://cran.r-project.org/package=mixR)

<!-- badges: end -->

# mixR: An R package for finite mixture modeling for both raw and binned data

## Why `mixR`?
R programming language provides a rich collection of packages for building and analyzing finite mixture models which are widely used in unsupervised learning such as model-based clustering and density estimation. For example, 
- [`mclust`](https://cran.r-project.org/web/packages/mclust/index.html) can be used to build Gaussian mixture models with different covariance structures
- [`mixtools`](https://cran.r-project.org/web/packages/mixtools/index.html) implements parametric and non-parametric mixture models as well as mixtures of Gaussian regressions
- [`flexmix`](https://cran.r-project.org/web/packages/flexmix/index.html) provides a general framework for finite mixtures of regression models
- [`mixdist`](https://cran.r-project.org/web/packages/mixdist/index.html) fits mixture models for grouped and conditional data (also called binned data). 

To our knowledge, almost all R packages for finite mixture models are designed to use raw data as the modeling input except `mixdist`. However the popular model selection methods based on information criteria or bootstrapping likelihood ratio test ([McLachlan, 1987](https://doi.org/10.2307/2347790); [Feng & McCulloch, 1996](https://doi.org/10.1111/j.2517-6161.1996.tb02104.x); [Yu & Harvill, 2019](https://doi.org/10.1080/03610926.2018.1494838)) are not implemented in `mixdist`.

`mixR` is a package that aims to bridge this gap and to unify the interface for finite mixture modeling for both raw and binned data.


## Installation

For stable/pre-compiled(for Windows and OS X) version, please install from [CRAN](https://CRAN.R-project.org/package=mixR):

```r
install.packages('mixR')
```

To get the latest development version from Github:
```r
# install.packages('devtools')
devtools::install_github('garybaylor/mixR')
```

## Examples

* Fitting a normal mixture model
```r
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
```r
# generate data from a Weibull mixture model
x2 = rmixweibull(1000, c(0.4, 0.6), c(0.6, 1.3), c(0.1, 0.1))
mod2_weibull = mixfit(x2, family = 'weibull', ncomp = 2)
```
* Fitting a mixture model with binned data
```r
head(Stamp2)
##     lower  upper freq
## 1  0.0595 0.0605    1
## 5  0.0635 0.0645    2
## 6  0.0645 0.0655    1
## 7  0.0655 0.0665    1
## 9  0.0675 0.0685    1
## 10 0.0685 0.0695    7
mod_binned = mixfit(Stamp2, ncomp = 7, family = 'weibull')
plot(mod_binned)

# data binned from numeric data
x1_binned = bin(x1, seq(min(x1), max(x1), length = 30))
mod1_binned = mixfit(x1_binned, ncomp = 2)
```

* Mixture model selection by BIC
```r
# Selecting the best g for Normal mixture model
s_normal = select(x2, ncomp = 2:6)

# Selecting the best g for Weibull mixture model
s_weibull = select(x2, ncomp = 2:6, family = 'weibull')

plot(s_weibull)
plot(s_normal)
```

* Mixture model selection by bootstrap likelihood ratio test (LRT)
```r
b1 = bs.test(x1, ncomp = c(2, 3))
plot(b1, main = 'Bootstrap LRT for Normal Mixture Models (g = 2 vs g = 3)')
b1$pvalue

b2 = bs.test(x2, ncomp = c(2, 4))
plot(b2, main = 'Bootstrap LRT for Normal Mixture Models (g = 2 vs g = 4)')
b2$pvalue
```
For more examples please check the vignette [An Introduction to mixR](https://cran.r-project.org/web/packages/mixR/vignettes/intro-to-mixr.pdf).


## Contributor Code of Conduct
Everyone is welcome to contribute to the project through reporting issues, posting feature requests, updating documentation, submitting pull requests, or contact the project maintainer directly. To maintain a friendly atmosphere and to collaborate in a fun and productive way, we expect contributors to abide by the [Contributor Code of Conduct](https://www.contributor-covenant.org/version/1/0/0/code-of-conduct/).
