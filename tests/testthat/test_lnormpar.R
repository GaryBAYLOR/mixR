test_that("Checking parameter conversion for log-normal", {
  skip_on_cran()
  skip_on_bioc()
  
  # from mu, sd to mulog and sdlog
  expect_equal(to_mulog_sdlog_lnorm(1, 1), list(mulog = -0.346573590279973, sdlog = 0.832554611157698), tolerance = 1e-3)
  expect_equal(to_mulog_sdlog_lnorm(1, 2), list(mulog = -0.80471895621705, sdlog = 1.26863624117952), tolerance = 1e-3)
  expect_equal(to_mulog_sdlog_lnorm(2, 1), list(mulog = 0.58157540490284, sdlog = 0.472380727077439), tolerance = 1e-3)
  expect_equal(to_mulog_sdlog_lnorm(2, 2), list(mulog = 0.346573590279973, sdlog = 0.832554611157698), tolerance = 1e-3)
  
  # from mulog, sdlog to mu and sd
  expect_equal(to_mu_sd_lnorm(-0.346573590279973, 0.832554611157698), list(mu = 1, sd = 1))
  expect_equal(to_mu_sd_lnorm(-0.80471895621705, 1.26863624117952), list(mu = 1, sd = 2))
  expect_equal(to_mu_sd_lnorm(0.58157540490284, 0.472380727077439), list(mu = 2, sd = 1))
  expect_equal(to_mu_sd_lnorm(0.346573590279973, 0.832554611157698), list(mu = 2, sd = 2))
})
