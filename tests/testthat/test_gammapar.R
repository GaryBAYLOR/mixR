test_that("Checking parameter conversion for gamma", {
  skip_on_cran()
  skip_on_bioc()
  
  # from alpha, lambda to mu and sd
  expect_equal(to_mu_sd_gamma(0.5, 1), list(mu = 0.5, sd = 0.707106781186548))
  expect_equal(to_mu_sd_gamma(0.5, 2), list(mu = 0.25, sd = 0.353553390593274))
  expect_equal(to_mu_sd_gamma(1, 1), list(mu = 1, sd = 1))
  expect_equal(to_mu_sd_gamma(1, 2), list(mu = 0.5, sd = 0.5))
  
  # from mu, sd to alpha and lambda
  expect_equal(to_shape_rate_gamma(0.5, 0.707106781186548), list(alpha = 0.5, lambda = 1))
  expect_equal(to_shape_rate_gamma(0.25, 0.353553390593274), list(alpha = 0.5, lambda = 2))
  expect_equal(to_shape_rate_gamma(1, 1), list(alpha = 1, lambda = 1))
  expect_equal(to_shape_rate_gamma(0.5, 0.5), list(alpha = 1, lambda = 2))
})
