test_that("Check if output is expected for different inputs", {
  skip_on_cran()
  skip_on_bioc()
  
  expect_error(CheckData(c('1', '2', '3'), 'normal'), "a numeric vector or matrix is required.")
  expect_error(CheckData(data.frame(x = 1:2, y = c('a', 'b')), 'normal'), "a numeric vector or matrix is required.")
  expect_error(CheckData(TRUE, 'normal'), "a numeric vector or matrix is required.")
  
  expect_error(CheckData(matrix(c(1, 2, 3, 4), ncol = 2), 'normal'), "the matrix should has three columns.")
  expect_error(CheckData(matrix(c(1.1, 1.2, 4, 1.2, 1.2, 5), ncol = 3, byrow = TRUE), 'normal'),
               "the first column of input matrix should be less than the second column.")
  expect_error(CheckData(matrix(c(1.1, 1.2, -1, 1.2, 1.3, 0), ncol = 3, byrow = TRUE), 'normal'),
               "the third column of input matrix should be positive and not all less than one.")
  
  expect_error(CheckData(matrix(c(1.1, 1.2, 4, 1.2, 1.3, 6), ncol = 3, byrow = TRUE), 'normal'),
               "insufficient data for model estimation.")
  expect_error(CheckData(rnorm(9), 'normal'),
               "insufficient data for model estimation.")
  expect_error(CheckData(c(-2, rnorm(30)), 'weibull'),
               "the data must be all positive.")
  
  expect_error(CheckData(c(NA, rnorm(30)), 'normal'),
               "Please remove NA/NaN/Inf in the input data.")
  expect_error(CheckData(c(NaN, rnorm(30)), 'normal'),
               "Please remove NA/NaN/Inf in the input data.")
  expect_error(CheckData(c(Inf, rnorm(30)), 'normal'),
               "Please remove NA/NaN/Inf in the input data.")
  
  
})