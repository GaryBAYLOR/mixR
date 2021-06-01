test_that("Checking parameter conversion for weibull", {
  skip_on_cran()
  skip_on_bioc()
  
  # from k, lambda to mu and sd
  expect_equal(to_mu_sd_weibull(0.5, 1), list(mu = 2, sd = 4.47213595499958), tolerance = 1e-3)
  expect_equal(to_mu_sd_weibull(0.5, 2), list(mu = 4, sd = 8.94427190999916), tolerance = 1e-3)
  expect_equal(to_mu_sd_weibull(1, 1), list(mu = 1, sd = 1), tolerance = 1e-3)
  expect_equal(to_mu_sd_weibull(1, 2), list(mu = 2, sd = 2), tolerance = 1e-3)
  
  # from mu, sd to k and lambda
  expect_equal(to_k_lambda_weibull(2, 4.47213595499958), list(k = 0.5, lambda = 1), tolerance = 1e-3)
  expect_equal(to_k_lambda_weibull(4, 8.94427190999916), list(k = 0.5, lambda = 2), tolerance = 1e-3)
  expect_equal(to_k_lambda_weibull(1, 1), list(k = 1, lambda = 1), tolerance = 1e-3)
  expect_equal(to_k_lambda_weibull(2, 2), list(k = 1, lambda = 2), tolerance = 1e-3)
})
