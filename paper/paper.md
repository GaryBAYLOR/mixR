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

-   `select()` selects the best model from a series of mixture models with different number of components using Bayesian Information Criterion (BIC). As studied in @steele2010, BIC achieves the best performance in mixture model selection compared to other information criteria.

-   `bs.test()` performs the likelihood ratio test by parametric bootstrapping [@efron1994] for two mixture models from the same distribution family but with different number of components.

`mixR` also contains the following additional features.

-   Visualization of the fitted mixture models using ggplot [@ggplot2].
-   Functions to generate random data from mixture models.
-   Functions to convert parameters of Weibull and Gamma mixture models between shape-scale representation used in probability density functions and mean-variance representation that is more intuitive for people to understand the distribution.

# Examples

We demonstrate how to use `mixR` for mixture model fitting and model selection.

## Model fitting

```{r}
library(mixR)
library(gridExtra)
set.seed(101)
x <- rmixweibull(1000, c(0.4, 0.6), c(0.6, 1.3), c(0.1, 0.1))

# fit a Normal mixture model with 2 components
mod1 <- mixfit(x, ncomp = 2)
p1 <- plot(mod1, title = 'Normal Mixture (2 components)')

# fit a Weibull mixture model with 2 components
mod2 <- mixfit(x, ncomp = 2, family = 'weibull')
p2 <- plot(mod2, title = 'Weibull Mixture (2 components)')

# fit a Normal mixture model after binning the raw data
x_binned <- bin(x, brks = seq(min(x), max(x), length = 30))
mod3 <- mixfit(x_binned, ncomp = 2)
p3 <- plot(mod3, title = 'Normal Mixture (binned data 3 components)')

# fitting a Normal mixture model with 3 components
mod4 <- mixfit(x, ncomp = 3)
p4 <- plot(mod4, title = 'Normal Mixture (3 components)')

grid.arrange(p1, p2, p3, p4, nrow = 2)
```

## Modeling selection

```{r}


```

# Summary

# References
