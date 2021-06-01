test_that("Check output and internal function calls when x is a vector", {
    skip_on_cran()
    skip_on_bioc()
    
    # Arrange
    m = mockery::mock(0, cycle = TRUE)
    m2 = mockery::mock(list(pi = 0, mu = 0, sd = 0, iter = 0, loglik = 0, 
                   aic = 0, bic = 0, data = 0, comp.prob = 0, family = "normal"),
                   cycle = TRUE)
    m3 = mockery::mock(0)
    
    mockery::stub(bs.test, 'rmixnormal', m)
    mockery::stub(bs.test, 'normalEM', m2)
    mockery::stub(bs.test, 'CheckData', m3)
    
    expected_output = structure(list(pvalue = 0, w0 = 0, w1 = rep(0, 10)),
                                class = 'bootEM')
    x = c(0, 1, 2)
    
    # Act
    actual_output = bs.test(x, ncomp = c(2, 3), B = 10)

    # Assert
    expect_equal(actual_output, expected_output)
    
    mockery::expect_called(m, 10)
    mockery::expect_called(m2, 2 * 10 + 2)
    mockery::expect_called(m3, 1)
})           

test_that("Check output and internal function calls when x is a matrix", {
    skip_on_cran()
    skip_on_bioc()
    
    # Arrange
    m = mockery::mock(0, cycle = TRUE)
    m2 = mockery::mock(list(pi = 0, mu = 0, sd = 0, iter = 0, loglik = 0, 
                            aic = 0, bic = 0, data = 0, comp.prob = 0, family = "normal"),
                       cycle = TRUE)
    m3 = mockery::mock(0)
    
    mockery::stub(bs.test, 'rmixnormal', m)
    mockery::stub(bs.test, 'normalEM2', m2)
    mockery::stub(bs.test, 'CheckData', m3)
    
    expected_output = structure(list(pvalue = 0, w0 = 0, w1 = rep(0, 10)),
                                class = 'bootEM')
    x = matrix(c(0, 0.1, 4, 2, 2.1, 4), ncol = 3, byrow = TRUE)
    
    # Act
    actual_output = bs.test(x, ncomp = c(2, 3), B = 10)
    
    # Assert
    expect_equal(actual_output, expected_output)
    
    mockery::expect_called(m, 10)
    mockery::expect_called(m2, 2 * 10 + 2)
    mockery::expect_called(m3, 1)
})           
