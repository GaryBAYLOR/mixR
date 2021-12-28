---
title: 'mixR: An R package for Finite Mixture Modeling for Raw and Binned Data'
tags:
  - R
  - mixture models
  - EM algorithm
  - model selection
authors:
  - name: Youjiao Yu
    affiliation: 1
affiliations:
  - name: Department of Statistical Science, Baylor University
    index: 1
date: "26 December 2021"
bibliography: paper.bib
---

# Statement of need

R programming language [@R] provides a rich collection of packages for building and analyzing finite mixture models which are widely used in unsupervised learning such as model-based clustering or density estimation for data that reveal different patterns. For example, `mclust` [@mclust] can be used to build Gaussian mixture models with different covariance structures, `mixtools` [@mixtools] implements parametric and non-parametric mixture models as well as mixtures of Gaussian regressions, `flexmix` [@flexmix] provides a general framework for finite mixtures of regression models, `mixdist` [@mixdist] fits Gaussian and non-Gaussian mixture models for grouped and conditional data. To our knowledge, all R packages for finite mixture models are designed to use raw data as the modeling input except `mixdist` which consumes grouped and conditional data. However the popular model selection methods based on information criteria or bootstrapping likelihood ratio test are not implemented in `mixdist`. To bridge this gap and to unify the interface for finite mixture modeling for both raw and grouped data, we implement `mixR` package that has the following primary functions.

-   `mixfit()` performs maximum likelihood estimation (MLE) for finite mixture models for Gaussian, Weibull, Gamma and Log-normal distribution via EM algorithm [@dempster1977]. The model fitting is accelerated via package `Rcpp` [@rcpp].

-   `select()` selects the best model from a series of mixture models with different number of components using Bayesian Information Criterion (BIC). @steele2010 show that BIC achieves the best performance in mixture model selection compared to other information criteria.

-   `bs.test()` performs the likelihood ratio test (LRT) by parametric bootstrapping [@efron1994] for two mixture models from the same distribution family but with different number of components.

`mixR` also contains the following additional features.

-   Visualization of the fitted mixture models using ggplot [@ggplot2].
-   Functions to generate random data from mixture models.
-   Functions to convert parameters of Weibull and Gamma mixture models between shape-scale representation used in probability density functions and mean-variance representation that is more intuitive for people to understand the distribution.

# Examples

We demonstrate how to use `mixR` for fitting finite mixture models and model selection using BIC and bootstrap LRT.

## Model fitting

We fit the following four mixture models to a data set with 1000 random data points generated from a Weibull mixture model with two components.

-   Gaussian mixture with two components (`mod1`)
-   Gaussian mixture with two components to the data binned from the raw data (`mod2`)
-   Gaussian mixture with three components (`mod3`)
-   Weibull mixture with two components (`mod4`)

\ref{fig:plot1} shows the above four mixture models, from which we see that binning does not cause much information loss and we can get similar fitted results from raw data and binned data. This is usually the case when we have at least moderate data size and the underlying mixture model is not too complex (e.g., too many components). A benefit of binning is it reduces the computation significantly for large data, especially when conducting bootstrap LRT which is computation intensive. Another thing we observe from \ref{fig;plot1} is Gaussian mixture model can provide a good fit for non-Gaussian data but the number of mixture components tends to be overestimated when the actual distribution is skewed.

```{r}
library(mixR)

set.seed(101)
x <- rmixweibull(1000, c(0.4, 0.6), c(0.6, 1.3), c(0.1, 0.1))
x_binned <- bin(x, brks = seq(min(x), max(x), length = 30))

mod1 <- mixfit(x, ncomp = 2)
mod2 <- mixfit(x_binned, ncomp = 2)
mod3 <- mixfit(x, ncomp = 3)
mod4 <- mixfit(x, ncomp = 2, family = 'weibull')

p1 <- plot(mod1, title = 'Gaussian Mixture (2 components)')
p2 <- plot(mod2, title = 'Gaussian Mixture (binned data 2 components)')
p3 <- plot(mod3, title = 'Gaussian Mixture (3 components)')
p4 <- plot(mod4, title = 'Weibull Mixture (2 components)')
gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)
```

![(top left) The fitted Gaussian mixture with two components; (top right) The fitted Gaussian mixture with two components to the data binned from the raw data; (bottom left) The fitted Gaussian mixture with three components; (bottom right) The fitted Weibull mixture with two components. \label{fig:plot1}](plot1.png)

## Model selection

\ref{fig:plot2} shows that by using BIC, the best Gaussian mixture model has three components and unequal variances for each component while the best Weibull mixture model has two components. The bootstrap LRT with $H_0: g=2$ against $H_a: g=3$ for Gaussian mixture models (using the default 100 bootstrap iterations) returns a p-value of zero, showing that Gaussian mixture with three components is significantly better than that with two components. Similarly the same test for Weibull mixture models returns a p-value of 0.82 which is insignificant and indicates Weibull mixture with three components is no better than that with two components.

```{r}
mod_selection_gaussian = select(x, ncomp = 2:4)
mod_selection_weibull = select(x, ncomp = 2:4, family = 'weibull')
b1 <- bs.test(x, ncomp = c(2, 3))
b2 <- bs.test(x, ncomp = c(2, 3), family = 'weibull')

b1$pvalue
## [1] 0

b2$pvalue
## [1] 0.82

par(mfrow = c(2, 2))
plot(mod_selection_gaussian)
plot(mod_selection_weibull, main="Weibull Mixture Model Selection by BIC")
plot(b1, main = "Bootstrap LRT for Gaussian Mixture Models\n (g = 2 vs g = 3)",
     xlab = 'Bootstrap Test Statistics')
plot(b2, main = "Bootstrap LRT for Weibull Mixture Models\n (g = 2 vs g = 3)",
     xlab = 'Bootstrap Test Statistics')
```

![(top left) Gaussian mixture model selection using BIC. UV stands for unequal variances for each mixture components and EV stands for equal variance; (top right) Weibull mixture model selection using BIC; (bottom left) Bootstrap LRT with $H_0: g=2$ against $H_a: g=3$ for Gaussian mixture models; (bottom right) Bootstrap LRT with $H_0: g=2$ against $H_a: g=3$ for Weibull mixture models \label{fig:plot2}](plot2.png)

# Summary

`mixR` unifies the interface for fitting and comparing finite mixture models for both raw data and binned data for families including Gaussian, Weibull, Gamma and Log-normal. The package also provide features for generating random numbers from mixture models, conversion of parameters for Weibull and Gamma models, and model visualization in `ggplot2`. The model fitting process is accelerated by `Rcpp`.

# References
