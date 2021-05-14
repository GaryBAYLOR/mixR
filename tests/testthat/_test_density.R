test_that("Check density output normal (unequal variance)", {
  ## densities (default)
  d = density(mod, smoothness = 64)
  
  # testing d$x
  expect_equal(length(d$x), 64)
  expect_equal(is.numeric(d$x), TRUE)
  
  # testing d$y
  expect_equal(all(d$y >= 0), TRUE)
  expect_equal(length(d$y), 64)
  
  # testing d$comp
  expect_equal(class(d$comp), c('matrix', 'array'))
  expect_equal(dim(d$comp), c(64, 3))
  expect_equal(apply(d$comp, 1, sum), d$y)
  
  
  ## density (at one location)
  dx = density(mod, at = 0)
  
  expect_equal(dx$x, 0)
  expect_equal(length(dx$y), 1)
  expect_gt(dx$y, 0)
  
  expect_equal(dim(dx$comp), c(1, 3))
  expect_equal(any(is.na(dx$comp)), FALSE)
  expect_equal(any(is.nan(dx$comp)), FALSE)
  
})

test_that("Check density output Gamma", {
  ## densities (default)
  d = density(mod2, smoothness = 64)
  
  # testing d$x
  expect_equal(length(d$x), 64)
  expect_equal(is.numeric(d$x), TRUE)
  
  # testing d$y
  expect_equal(all(d$y >= 0), TRUE)
  expect_equal(length(d$y), 64)
  
  # testing d$comp
  expect_equal(class(d$comp), c('matrix', 'array'))
  expect_equal(dim(d$comp), c(64, 2))
  expect_equal(apply(d$comp, 1, sum), d$y)
  
  
  ## density (at one location)
  dx = density(mod2, at = -0.01)
  
  expect_equal(dx$x, -0.01)
  expect_equal(length(dx$y), 1)
  expect_equal(dx$y, 0)  # the density when x < 0 should be 0
  
  expect_equal(dim(dx$comp), c(1, 2))
  expect_equal(any(is.na(dx$comp)), FALSE)
  expect_equal(any(is.nan(dx$comp)), FALSE)
  
})


test_that("Check density output Log-normal", {
  ## densities (default)
  d = density(mod3, smoothness = 64)
  
  # testing d$x
  expect_equal(length(d$x), 64)
  expect_equal(is.numeric(d$x), TRUE)
  
  # testing d$y
  expect_equal(all(d$y >= 0), TRUE)
  expect_equal(length(d$y), 64)
  
  # testing d$comp
  expect_equal(class(d$comp), c('matrix', 'array'))
  expect_equal(dim(d$comp), c(64, 2))
  expect_equal(apply(d$comp, 1, sum), d$y)
  
  
  ## density (at one location)
  dx = density(mod3, at = -0.01)
  
  expect_equal(dx$x, -0.01)
  expect_equal(length(dx$y), 1)
  expect_equal(dx$y, 0)  # the density when x < 0 should be 0
  
  expect_equal(dim(dx$comp), c(1, 2))
  expect_equal(any(is.na(dx$comp)), FALSE)
  expect_equal(any(is.nan(dx$comp)), FALSE)
  
})


test_that("Check density output Weibull", {
  ## densities (default)
  d = density(mod4, smoothness = 64)
  
  # testing d$x
  expect_equal(length(d$x), 64)
  expect_equal(is.numeric(d$x), TRUE)
  
  # testing d$y
  expect_equal(all(d$y >= 0), TRUE)
  expect_equal(length(d$y), 64)
  
  # testing d$comp
  expect_equal(class(d$comp), c('matrix', 'array'))
  expect_equal(dim(d$comp), c(64, 2))
  expect_equal(apply(d$comp, 1, sum), d$y)
  
  
  ## density (at one location)
  dx = density(mod4, at = -0.01)
  
  expect_equal(dx$x, -0.01)
  expect_equal(length(dx$y), 1)
  expect_equal(dx$y, 0)  # the density when x < 0 should be 0
  
  expect_equal(dim(dx$comp), c(1, 2))
  expect_equal(any(is.na(dx$comp)), FALSE)
  expect_equal(any(is.nan(dx$comp)), FALSE)
  
})
