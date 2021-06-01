test_that("Check output properties", {
  
  skip_on_cran()
  skip_on_bioc()
  
  x = rnorm(20)
  res = bin(x, seq(min(x), max(x), length = 5))
  
  expect_equal(ncol(res), 3)
  expect_equal(sum(res[, 3]), 20)
  expect_equal(class(res), c('matrix', 'array'))
})