## K-means clustering

# INPUT:
#     x: a data frame of dimention n by p where n is size of data and p is the number of variables
#     k: the number of clusters
k_means = function(x, k) {
  
  # setting
  n = nrow(x)
  p = ncol(x)
  mu = apply(x, 2, mean)
  sd = apply(x, 2, sd)
  
  # step0: initialize cluster centers (centroids)
  center = matrix(0, nrow = k, ncol = p)    # k by p
  for (j in 1:p) {
    center[, j] = rnorm(k, mu[j], sd[j])
  }
  
  distance = matrix(0, nrow = n, ncol = k)
  dist = function(data, center) {
    k = nrow(center)
    result = numeric(k)
    for (i in 1:k) {
      result[i] = sqrt(sum((data - center[i, ])^2))
    }
    return(result)
  }
  iter = 0
  repeat {
      # step1: given the center of each cluster, refresh the cluster labels
      for (i in 1:n) {
        distance[i, ] = dist(x[i, ], center)
      }
      label = apply(distance, 1, which.min)
    
      # step2: find out center of each cluster (update the matrix 'center')
      for (j in 1:k) {
        center[j, ] = apply(x[label == j, ], 2, mean)
      }
      iter = iter + 1
      if (iter > 20) break
  }
  
  res = list(data = x, label = label, center = center, k = k)
  class(res) = "kmeans"
  return(res)
}

plot.kmeans = function(object) {
  plot(object$data, type = "n", xlab = "X1", ylab = "X2", main = "K-means Clustering")
  for (i in 1:object$k) {
    points(object$data[object$label == i, ], col = i, pch = 19)
  }
}

x = rbind(matrix(rnorm(50*2, 2, 0.4), nrow = 50, ncol = 2),
          matrix(rnorm(80*2, 0, 0.5), nrow = 80, ncol = 2),
          matrix(rnorm(30*2, -2, 0.3), nrow = 30, ncol = 2)
          )
x = x[sample(nrow(x)), ]


obj = k_means(x, k = 3)

plot(obj)
