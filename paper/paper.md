---
title: mixR: An R package for Finite Mixture Modeling for Raw and Binned Data
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
date: 26 December 2021
bibliography: paper.bib
---

# Summary
R programming language provides a rich collection of packages for building and analyzing finite mixture models which are widely used in unsupervised learning such as model-based clustering or density estimation, for solving different academia or industrial problems. For example, mclust (2016) can be used to build Gaussian mixture models with different covariance structures, mixtools (2009) implements functionalities for parametric and non-parametric mixtures as well as mixtures of Gaussian regressions, flexmix () provides a general framework for finite mixtures of regression models, mixdist () fits Gaussian and non-Gaussian mixture models for grouped data. A comprehensive list of R packages for finite mixture models can be found in CRAN Task View: Cluster Analysis & Finite Mixture Models (https://cran.r-project.org/web/views/Cluster.html). 


To our knowledge, all R packages for finite mixture models consumes raw data except mixdist which uses grouped conditional data as the model input. As discussed in Yu (2018), there are situations when data used for mixture models are in the form of grouped or binned data for which we need to conduct model comparison and selection basides fitting a mixture model. However the popular model selection methods based on information criteria or bootstrapping are not implemented in mixdist. To bridge this gap and to unify the interface for finite mixture model analysis for both raw and binned data, we implement the mixR package that has the following main features.

* Performs maximum likelihood estimation (MLE) for finite mixture models for Gaussian, Weibull, Gamma and Log-normal distribution via EM algorithm

* Conducts mixture model selection by using Bayesian Information Criterion (BIC) or bootstrap likelihood ratio test (LRT). 
mix

mixR also contains the following additional features
* Improves the visualization of the fitted mixture models using ggplot ().
* Provides functions to generate random data from mixture models.
* Provides functions to convert parameters of Weibull and Gamma mixture models between shape-scale representation that is used in probability density functions and mean-variance representation that is more intuitive for people to understand the distribution.



# Statement of need



# A Minimal Example



# References
