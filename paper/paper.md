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

R programming language [@R] provides a rich collection of packages for building and analyzing finite mixture models which are widely used in unsupervised learning such as model-based clustering or density estimation for data that reveal different patterns. For example, `mclust` [@mclust] can be used to build Gaussian mixture models with different covariance structures, `mixtools` [@mixtools] implements parametric and non-parametric mixture models as well as mixtures of Gaussian regressions, `flexmix` [@flexmix] provides a general framework for finite mixtures of regression models, `mixdist` [@mixdist] fits Gaussian and non-Gaussian mixture models for grouped and conditional data. To our knowledge, all R packages for finite mixture models are designed to use raw data as the modeling input except `mixdist` which consumes grouped and conditional data. However the popular model selection methods based on information criteria or bootstrapping likelihood ratio test (LRT) are not implemented in `mixdist`. We implement `mixR` package to bridge this gap and to unify the interface for finite mixture modeling for both raw and grouped data.


# Summary

that has the following main features.

As discussed in Yu (2018), there are situations when data used for mixture models are in the form of grouped or binned data for which we need to conduct model comparison and selection besides fitting a mixture model. 

- Performs maximum likelihood estimation (MLE) for finite mixture models for Gaussian, Weibull, Gamma and Log-normal distribution via EM algorithm

-   Conducts mixture model selection by using Bayesian Information Criterion (BIC) or bootstrap likelihood ratio test (LRT). mix

mixR also contains the following additional features \* Improves the visualization of the fitted mixture models using ggplot (). \* Provides functions to generate random data from mixture models. \* Provides functions to convert parameters of Weibull and Gamma mixture models between shape-scale representation that is used in probability density functions and mean-variance representation that is more intuitive for people to understand the distribution.


# Statement of need


# A Minimal Example


# References
