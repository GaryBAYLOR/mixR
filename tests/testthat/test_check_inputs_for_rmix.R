test_that("Check if output is correct for rmix functions", {
  skip_on_cran()
  skip_on_bioc()
  
  # test missing inputs
  expect_error(check_inputs_for_rmix(100, c(0.4, 0.6), c(1, 2)), 
               "'n', 'pi', 'mu' and 'sd' must be all provided!")
  expect_error(check_inputs_for_rmix(pi = c(0.4, 0.6), mu = c(1, 2), sd = c(1, 1)),
               "'n', 'pi', 'mu' and 'sd' must be all provided!")
  
  # test non-numeric inputs
  expect_error(check_inputs_for_rmix(n = 100, pi = matrix(c(0.4, 0.6)), mu = c(1, 2), sd = c(1, 1)),
               "'n', 'pi', 'mu', 'sd' must be numeric!")
  expect_error(check_inputs_for_rmix(n = '100', pi = c(0.4, 0.6), mu = c(1, 2), sd = c(1, 1)),
               "'n', 'pi', 'mu', 'sd' must be numeric!")
  expect_error(check_inputs_for_rmix(n = 100, pi = c(0.4, 0.6), mu = list(c(1, 2)), sd = c(1, 1)),
               "'n', 'pi', 'mu', 'sd' must be numeric!")
  
  # test inputs of wrong lengths
  expect_error(check_inputs_for_rmix(n = c(100, 100), pi = c(0.4, 0.6), mu = c(1, 2), sd = c(1, 1)),
               "'n' must be a scalar!")
  expect_error(check_inputs_for_rmix(n = 100, pi = c(0.4, 0.6), mu = c(1, 2), sd = c(1, 1, 1)),
               "'pi', 'mu' and 'sd' should all have the same length!")
  expect_error(check_inputs_for_rmix(n = 100, pi = 1, mu = c(1, 2), sd = c(1, 1, 1)),
               "'pi', 'mu' and 'sd' should all have the same length!")
  
  # test inputs that contains NA or NaN
  expect_error(check_inputs_for_rmix(n = 100, pi = c(0.4, 0.6), mu = c(1, NA), sd = c(1, 1)),
               "'n', 'pi', 'mu' and 'sd' can not contain missing values!")
  expect_error(check_inputs_for_rmix(n = 100, pi = c(NaN, NaN), mu = c(1, 1), sd = c(1, 1)),
               "'n', 'pi', 'mu' and 'sd' can not contain missing values!")
  
  # test negative inputs
  expect_error(check_inputs_for_rmix(n = 100, pi = c(0.4, 0.6), mu = c(1, 1), sd = c(-1, 1)),
               "'n', 'pi' and 'sd' should all be positive and 'n' should be at least 1!")
  expect_error(check_inputs_for_rmix(n = 0, pi = c(0.4, 0.6), mu = c(1, 1), sd = c(1, 1)),
               "'n', 'pi' and 'sd' should all be positive and 'n' should be at least 1!")
  expect_error(check_inputs_for_rmix(n = 0, pi = c(0.4, -0.6), mu = c(1, 1), sd = c(1, 1)),
               "'n', 'pi' and 'sd' should all be positive and 'n' should be at least 1!")
  
  # test inputs out of rqnge
  expect_error(check_inputs_for_rmix(n = 100, pi = c(0.4, 0.6), mu = c(-2e6, 1), sd = c(1, 1)),
               "'mu' is out of range!")
  
})