test_that("Check if normalEM() is called when x is a vector", {
  # Arrange
  m = mockery::mock(0)
  m1 = mockery::mock(list(aic = 0, bic = 0))
  m2 = mockery::mock(list(aic = 0, bic = 0))
  
  mockery::stub(mixfit, 'CheckData', m)
  mockery::stub(mixfit, 'normalEM', m1)
  mockery::stub(mixfit, 'normalEM2', m2)
  x = c(1, 2, 3)
  expected_output = list(aic = 0, bic = 0)
  
  # Act
  actual_output = mixfit(x, ncomp = c(2, 3))
  
  # Assert
  expect_equal(actual_output, expected_output)
  mockery::expect_called(m, 1)
  mockery::expect_called(m1, 1)
  mockery::expect_called(m2, 0)
})

test_that("Check if normalEM2() is called when x is a matrix", {
  # Arrange
  m = mockery::mock(0)
  m1 = mockery::mock(list(aic = 0, bic = 0))
  m2 = mockery::mock(list(aic = 0, bic = 0))
  
  mockery::stub(mixfit, 'CheckData', m)
  mockery::stub(mixfit, 'normalEM', m1)
  mockery::stub(mixfit, 'normalEM2', m2)
  x = matrix(c(1, 2, 3, 4, 5, 6), ncol = 3)
  expected_output = list(aic = 0, bic = 0)
  
  # Act
  actual_output = mixfit(x, ncomp = c(2, 3))
  
  # Assert
  expect_equal(actual_output, expected_output)
  mockery::expect_called(m, 1)
  mockery::expect_called(m1, 0)
  mockery::expect_called(m2, 1)
})


test_that("Check if mixfit() returns NULL when aic is NA or NaN", {
  # Arrange
  m = mockery::mock(0, cycle = TRUE)
  m1 = mockery::mock(list(aic = 0, bic = NA), cycle = TRUE)
  m2 = mockery::mock(list(aic = 0, bic = NaN), cycle = TRUE)
  
  mockery::stub(mixfit, 'CheckData', m)
  mockery::stub(mixfit, 'normalEM', m1)
  mockery::stub(mixfit, 'normalEM2', m2)
  
  expected_output = NULL
  
  # Act
  actual_output1 = mixfit(c(1, 2, 3), ncomp = c(2, 3))
  actual_output2 = mixfit(matrix(1:6, ncol = 3), ncomp = c(2, 3))
  
  # Assert
  expect_equal(actual_output1, expected_output)
  expect_equal(actual_output2, expected_output)
  mockery::expect_called(m, 2)
  mockery::expect_called(m1, 5)
  mockery::expect_called(m2, 5)
})


test_that("Check if gammaEM() is called when x is a vector", {
  # Arrange
  m = mockery::mock(0)
  m1 = mockery::mock(list(aic = 0, bic = 0))
  m2 = mockery::mock(list(aic = 0, bic = 0))
  
  mockery::stub(mixfit, 'CheckData', m)
  mockery::stub(mixfit, 'gammaEM', m1)
  mockery::stub(mixfit, 'gammaEM2', m2)
  x = c(1, 2, 3)
  expected_output = list(aic = 0, bic = 0)
  
  # Act
  actual_output = mixfit(x, ncomp = c(2, 3), family = 'gamma')
  
  # Assert
  expect_equal(actual_output, expected_output)
  mockery::expect_called(m, 1)
  mockery::expect_called(m1, 1)
  mockery::expect_called(m2, 0)
})


test_that("Check if gammaEM2() is called when x is a matrix", {
  # Arrange
  m = mockery::mock(0)
  m1 = mockery::mock(list(aic = 0, bic = 0))
  m2 = mockery::mock(list(aic = 0, bic = 0))
  
  mockery::stub(mixfit, 'CheckData', m)
  mockery::stub(mixfit, 'gammaEM', m1)
  mockery::stub(mixfit, 'gammaEM2', m2)
  x = matrix(1:6, ncol = 3)
  expected_output = list(aic = 0, bic = 0)
  
  # Act
  actual_output = mixfit(x, ncomp = c(2, 3), family = 'gamma')
  
  # Assert
  expect_equal(actual_output, expected_output)
  mockery::expect_called(m, 1)
  mockery::expect_called(m1, 0)
  mockery::expect_called(m2, 1)
})


test_that("Check if lnormEM() is called when x is a vector", {
  # Arrange
  m = mockery::mock(0)
  m1 = mockery::mock(list(aic = 0, bic = 0))
  m2 = mockery::mock(list(aic = 0, bic = 0))
  
  mockery::stub(mixfit, 'CheckData', m)
  mockery::stub(mixfit, 'lnormEM', m1)
  mockery::stub(mixfit, 'lnormEM2', m2)
  x = c(1, 2, 3)
  expected_output = list(aic = 0, bic = 0)
  
  # Act
  actual_output = mixfit(x, ncomp = c(2, 3), family = 'lnorm')
  
  # Assert
  expect_equal(actual_output, expected_output)
  mockery::expect_called(m, 1)
  mockery::expect_called(m1, 1)
  mockery::expect_called(m2, 0)
})


test_that("Check if lnormEM2() is called when x is a matrix", {
  # Arrange
  m = mockery::mock(0)
  m1 = mockery::mock(list(aic = 0, bic = 0))
  m2 = mockery::mock(list(aic = 0, bic = 0))
  
  mockery::stub(mixfit, 'CheckData', m)
  mockery::stub(mixfit, 'lnormEM', m1)
  mockery::stub(mixfit, 'lnormEM2', m2)
  x = matrix(1:6, ncol = 3)
  expected_output = list(aic = 0, bic = 0)
  
  # Act
  actual_output = mixfit(x, ncomp = c(2, 3), family = 'lnorm')
  
  # Assert
  expect_equal(actual_output, expected_output)
  mockery::expect_called(m, 1)
  mockery::expect_called(m1, 0)
  mockery::expect_called(m2, 1)
})


test_that("Check if weibullEM() is called when x is a vector", {
  # Arrange
  m = mockery::mock(0)
  m1 = mockery::mock(list(aic = 0, bic = 0))
  m2 = mockery::mock(list(aic = 0, bic = 0))
  
  mockery::stub(mixfit, 'CheckData', m)
  mockery::stub(mixfit, 'weibullEM', m1)
  mockery::stub(mixfit, 'weibullEM2', m2)
  x = c(1, 2, 3)
  expected_output = list(aic = 0, bic = 0)
  
  # Act
  actual_output = mixfit(x, ncomp = c(2, 3), family = 'weibull')
  
  # Assert
  expect_equal(actual_output, expected_output)
  mockery::expect_called(m, 1)
  mockery::expect_called(m1, 1)
  mockery::expect_called(m2, 0)
})


test_that("Check if weibullEM2() is called when x is a matrix", {
  # Arrange
  m = mockery::mock(0)
  m1 = mockery::mock(list(aic = 0, bic = 0))
  m2 = mockery::mock(list(aic = 0, bic = 0))
  
  mockery::stub(mixfit, 'CheckData', m)
  mockery::stub(mixfit, 'weibullEM', m1)
  mockery::stub(mixfit, 'weibullEM2', m2)
  x = matrix(1:6, ncol = 3)
  expected_output = list(aic = 0, bic = 0)
  
  # Act
  actual_output = mixfit(x, ncomp = c(2, 3), family = 'weibull')
  
  # Assert
  expect_equal(actual_output, expected_output)
  mockery::expect_called(m, 1)
  mockery::expect_called(m1, 0)
  mockery::expect_called(m2, 1)
})

