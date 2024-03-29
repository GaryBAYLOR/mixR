% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/initz.R
\name{initz}
\alias{initz}
\title{Initialization of the EM Algorithm}
\usage{
initz(x, ncomp, init.method = c("kmeans", "hclust"))
}
\arguments{
\item{x}{a numeric vector of the raw data or a three-column matrix of the binned data}

\item{ncomp}{a positive integer specifying the number of components for a mixture model}

\item{init.method}{the method used for providing initial values, which can be one of
\code{kmeans} or \code{hclust}.}
}
\value{
\code{initz} returns a list with three items
\item{pi }{a numeric vector of component proportions}
\item{mu }{a numeric vector of component means}
\item{sd }{a numeric vector of component standard deviations}
}
\description{
This function returns the mean and standard deviation of each component by using
K-means clustering or hierarchical clustering.
}
\details{
The function \code{initz} returns the mean and standard deviation of each component
of a mixture model by using K-means clustering algorithm, or hierarchical clustering
method. It is used for automatically selecting initial values for the EM algorithm,
so as to enable mixture model selection by bootstrapping likelihood ratio test or
using information criteria.
}
\examples{
x <- rmixnormal(500, c(0.5, 0.5), c(2, 5), c(1, 0.7))
data <- bin(x, seq(-2, 8, 0.25))
par1 <- initz(x, 2)
par2 <- initz(data, 2)

}
