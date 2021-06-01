## ========================================= NORMAL ==========================================

test_that("Check normal density at one location", {
  skip_on_cran()
  skip_on_bioc()
  
  # Arrange
  x = structure(list(pi = c(0.5, 0.5), mu = c(0, 2), sd = c(1, 1), iter = 0, loglik = 0, 
                        aic = 0, bic = 0, data = 0, comp.prob = 0, family = "normal"),
                  class = 'mixfitEM')
  at = 0
  expected_output = structure(list(x = 0, y = 0.22646662345731, 
                                   comp = structure(c(0.199471140200716, 0.026995483256594), 
                                                    .Dim = 1:2, 
                                                    .Dimnames = list(NULL, c("Comp1", "Comp2")))), 
                              class = "densityEM")

  # Act
  actual_output = density(x, at)
  
  # Assert
  expect_equal(actual_output, expected_output)
})


test_that("Check normal densities at multiple locations", {
  skip_on_cran()
  skip_on_bioc()
  
  # Arrange
  x = structure(list(pi = c(0.5, 0.5), mu = c(0, 2), sd = c(1, 1), iter = 0, loglik = 0, 
                     aic = 0, bic = 0, data = 0, comp.prob = 0, family = "normal"),
                class = 'mixfitEM')
  at = c(-1, 0, 1, 2)
  expected_output = structure(list(x = c(-1, 0, 1, 2), 
                                   y = c(0.123201286465541, 0.22646662345731, 0.241970724519143, 0.22646662345731),
                                   comp = structure(c(0.120985362259572, 0.199471140200716, 0.120985362259572, 
                                                      0.026995483256594, 0.002215924205969, 0.026995483256594, 0.120985362259572, 
                                                      0.199471140200716), 
                                                    .Dim = c(4L, 2L), 
                                                    .Dimnames = list(NULL, c("Comp1", "Comp2")))), 
                              class = 'densityEM')
  
  # Act
  actual_output = density.mixfitEM(x, at)
  
  # Assert
  expect_equal(actual_output, expected_output)
})


test_that("Check normal densities when at is missing", {
  skip_on_cran()
  skip_on_bioc()
  
  # Arrange
  x = structure(list(pi = c(0.5, 0.5), mu = c(0, 2), sd = c(1, 1), iter = 0, loglik = 0, 
                     aic = 0, bic = 0, data = 0, comp.prob = 0, family = "normal"),
                class = 'mixfitEM')
  expected_output = structure(list(x = c(-3.8, -1.4, 1, 3.4, 5.8), 
                                   y = c(0.000145983349493933, 0.0754798424021089, 0.241970724519143, 
                                         0.075479842402109, 0.000145983349493933), 
                                   comp = structure(c(0.00014597346289573, 0.0748637328178724, 
                                                      0.120985362259572, 0.000616109584236511, 9.88659820312235e-09, 
                                                      9.88659820312235e-09, 0.00061610958423651, 0.120985362259572, 
                                                      0.0748637328178725, 0.00014597346289573), 
                                                    .Dim = c(5L, 2L), 
                                                    .Dimnames = list(NULL, c("Comp1", "Comp2")))), 
                              class = "densityEM")
  
  # Act
  actual_output = density(x, smoothness = 5, cut = 3.8)
  
  # Assert
  expect_equal(actual_output, expected_output)
})


## ========================================= GAMMA ==========================================

test_that("Check Gamma density at one location", {
  skip_on_cran()
  skip_on_bioc()
  
  # Arrange
  x = structure(list(pi = c(0.964748246177737, 0.0352517538222629), 
                     mu = c(1.51347806919324, 5.20375520327911), 
                     sd = c(0.767379254045787, 0.950021890505302), 
                     alpha = c(3.88984375, 30.003125), 
                     lambda = c(2.57013552371689, 5.7656680277915), 
                     loglik = 0, iter = 0, aic = 0, bic = 0, 
                     data = 0, comp.prob = 0, family = "gamma"), 
                class = "mixfitEM")
  at = 0
  expected_output = structure(list(x = 0, y = 0, comp = structure(c(0, 0), .Dim = 1:2)), class = "densityEM")
  
  # Act
  actual_output = density(x, at)
  
  # Assert
  expect_equal(actual_output, expected_output)
})


test_that("Check Gamma densities at multiple locations", {
  skip_on_cran()
  skip_on_bioc()
  
  # Arrange
  x = structure(list(pi = c(0.964748246177737, 0.0352517538222629), 
                     mu = c(1.51347806919324, 5.20375520327911), 
                     sd = c(0.767379254045787, 0.950021890505302), 
                     alpha = c(3.88984375, 30.003125), 
                     lambda = c(2.57013552371689, 5.7656680277915), 
                     loglik = 0, iter = 0, aic = 0, bic = 0, 
                     data = 0, comp.prob = 0, family = "gamma"), 
                class = "mixfitEM")
  at = c(0, 1, 2, 3)
  expected_output = structure(list(x = c(0, 1, 2, 3), 
                                   y = c(0, 0.554714106877258, 0.314633615960801, 0.0782734710821143), 
                                   comp = structure(c(0, 0.554714106876426, 0.314632214018291, 0.0777112214325133, 
                                                      0, 8.31605629451252e-13, 1.40194250965946e-06, 0.000562249649601007), 
                                                    .Dim = c(4L, 2L))), 
                              class = "densityEM")
  
  # Act
  actual_output = density(x, at)
  
  # Assert
  expect_equal(actual_output, expected_output)
})


test_that("Check Gamma densities when at is missing", {
  skip_on_cran()
  skip_on_bioc()
  
  # Arrange
  x = structure(list(pi = c(0.964748246177737, 0.0352517538222629), 
                     mu = c(1.51347806919324, 5.20375520327911), 
                     sd = c(0.767379254045787, 0.950021890505302), 
                     alpha = c(3.88984375, 30.003125), 
                     lambda = c(2.57013552371689, 5.7656680277915), 
                     loglik = 0, iter = 0, aic = 0, bic = 0, 
                     data = 0, comp.prob = 0, family = "gamma"), 
                class = "mixfitEM")
  expected_output = structure(list(x = c(0, 2.29371167639782, 4.58742335279564, 6.88113502919345, 9.17484670559127), 
                                   y = c(0, 0.219766660240038, 0.0178060036006819, 0.0031182586124188, 2.36129099992403e-05), 
                                   comp = structure(c(0, 0.219752944512847, 0.00448366648144676, 3.98362519035975e-05, 2.51826159683671e-07, 
                                                      0, 1.37157271901813e-05, 0.0133223371192352, 0.0030784223605152, 2.33610838395566e-05), 
                                                    .Dim = c(5L, 2L))), 
                              class = "densityEM")
  
  # Act
  actual_output = density(x, smoothness = 5, cut = 3.8)
  
  # Assert
  expect_equal(actual_output, expected_output)
})


## ======================================= LOG-NORMAL ==========================================

test_that("Check Log-normal density at one location", {
  skip_on_cran()
  skip_on_bioc()
  
  # Arrange
  x = structure(list(pi = c(0.5688314414468, 0.4311685585532), 
                     mu = c(0.421211689536832, 2.03710424516956), 
                     sd = c(0.411278350028963, 0.992138312228819), 
                     mulog = c(-1.19942046660724, 0.605095315327994), 
                     sdlog = c(0.818259492725197, 0.461363869022234), 
                     loglik = 0, iter = 0, aic = 0, bic = 0, data = 0, 
                     comp.prob = 0, family = "lnorm"), 
                class = "mixfitEM")
  at = 0
  expected_output = structure(list(x = 0, y = 0, comp = structure(c(0, 0), .Dim = 1:2)), class = "densityEM")
  
  # Act
  actual_output = density(x, at)
  
  # Assert
  expect_equal(actual_output, expected_output)
})


test_that("Check Log-normal density at multiple locations", {
  skip_on_cran()
  skip_on_bioc()
  
  # Arrange
  x = structure(list(pi = c(0.5688314414468, 0.4311685585532), 
                     mu = c(0.421211689536832, 2.03710424516956), 
                     sd = c(0.411278350028963, 0.992138312228819), 
                     mulog = c(-1.19942046660724, 0.605095315327994), 
                     sdlog = c(0.818259492725197, 0.461363869022234), 
                     loglik = 0, iter = 0, aic = 0, bic = 0, data = 0, 
                     comp.prob = 0, family = "lnorm"), 
                class = "mixfitEM")
  at = c(0, 1, 2, 3)
  expected_output = structure(list(x = c(0, 1, 2, 3), 
                                   y = c(0, 0.252476932505384, 0.192608903962779, 0.0719245247293207), 
                                   comp = structure(c(0, 0.0947184149086852, 0.00955703800527375, 0.00179129935758665, 
                                                      0, 0.157758517596699, 0.183051865957505, 0.070133225371734), 
                                                    .Dim = c(4L, 2L))), 
                              class = "densityEM")
  
  # Act
  actual_output = density(x, at)
  
  # Assert
  expect_equal(actual_output, expected_output)
})


test_that("Check Log-normal densities when at is missing", {
  skip_on_cran()
  skip_on_bioc()
  
  # Arrange
  x = structure(list(pi = c(0.5688314414468, 0.4311685585532), 
                     mu = c(0.421211689536832, 2.03710424516956), 
                     sd = c(0.411278350028963, 0.992138312228819), 
                     mulog = c(-1.19942046660724, 0.605095315327994), 
                     sdlog = c(0.818259492725197, 0.461363869022234), 
                     loglik = 0, iter = 0, aic = 0, bic = 0, data = 0, 
                     comp.prob = 0, family = "lnorm"), 
                class = "mixfitEM")
  expected_output = structure(list(x = c(0, 1.54606059757151, 3.09212119514301, 4.63818179271452, 6.18424239028602), 
                                   y = c(0, 0.24979198905991, 0.0648650655713017, 0.0108007960410826, 0.00190907349370277), 
                                   comp = structure(c(0, 0.0243590459747201, 0.00156550071110365, 0.000225375328290281, 4.9091501030397e-05, 
                                                      0, 0.22543294308519, 0.063299564860198, 0.0105754207127923, 0.00185998199267237), 
                                                    .Dim = c(5L, 2L))), 
                              class = "densityEM")
  
  # Act
  actual_output = density(x, smoothness = 5, cut = 3.8)
  
  # Assert
  expect_equal(actual_output, expected_output)
})


## ======================================= LOG-NORMAL ==========================================

test_that("Check Weibull density at one location", {
  skip_on_cran()
  skip_on_bioc()
  
  # Arrange
  x = structure(list(pi = c(0.383872774565691, 0.616127225434309), 
                     mu = c(1.0096215101336, 3.96223702267092), 
                     sd = c(0.501979260128999, 0.816364618548761), 
                     k = c(2.11455078125, 5.6128662109375), 
                     lambda = c(1.13996828318297, 4.28690932187183), 
                     iter = 0, loglik = 0, aic = 0, bic = 0, data = 0, comp.prob = 0, family = "weibull"), 
                class = "mixfitEM")
  at = 0
  expected_output = structure(list(x = 0, y = 0, comp = structure(c(0, 0), .Dim = 1:2)), class = "densityEM")
  
  # Act
  actual_output = density(x, at)
  
  # Assert
  expect_equal(actual_output, expected_output)
})


test_that("Check Weibull densities at multiple locations", {
  skip_on_cran()
  skip_on_bioc()
  
  # Arrange
  x = structure(list(pi = c(0.383872774565691, 0.616127225434309), 
                     mu = c(1.0096215101336, 3.96223702267092), 
                     sd = c(0.501979260128999, 0.816364618548761), 
                     k = c(2.11455078125, 5.6128662109375), 
                     lambda = c(1.13996828318297, 4.28690932187183), 
                     iter = 0, loglik = 0, aic = 0, bic = 0, data = 0, comp.prob = 0, family = "weibull"), 
                class = "mixfitEM")
  at = c(0, 1, 2, 3)
  expected_output = structure(list(x = c(0, 1, 2, 3), 
                                   y = c(0, 0.289306176245229, 0.0736166111558951, 0.136757757201094), 
                                   comp = structure(c(0, 0.288327608722764, 0.0499950199838413, 0.000913214870257778,
                                                      0, 0.000978567522464118, 0.0236215911720538, 0.135844542330836), 
                                                    .Dim = c(4L, 2L))), 
                              class = "densityEM")
  
  # Act
  actual_output = density(x, at)
  
  # Assert
  expect_equal(actual_output, expected_output)
})


test_that("Check Weibull densities when at is missing", {
  skip_on_cran()
  skip_on_bioc()
  
  # Arrange
  x = structure(list(pi = c(0.383872774565691, 0.616127225434309), 
                     mu = c(1.0096215101336, 3.96223702267092), 
                     sd = c(0.501979260128999, 0.816364618548761), 
                     k = c(2.11455078125, 5.6128662109375), 
                     lambda = c(1.13996828318297, 4.28690932187183), 
                     iter = 0, loglik = 0, aic = 0, bic = 0, data = 0, comp.prob = 0, family = "weibull"), 
                class = "mixfitEM")
  expected_output = structure(list(x = c(0, 1.84366028205119, 3.68732056410237, 5.53098084615356, 7.37464112820474), 
                                   y = c(0, 0.0930376708034856, 0.262103686406491, 0.0400033349358285, 7.41466413271976e-09), 
                                   comp = structure(c(0, 0.0767276462873693, 1.67077551444845e-05, 2.32224952135639e-12, 1.76541986879864e-22, 
                                                      0, 0.0163100245161162, 0.262086978651347, 0.0400033349335062, 7.41466413271958e-09), 
                                                    .Dim = c(5L, 2L))), 
                              class = "densityEM")
  
  # Act
  actual_output = density(x, smoothness = 5, cut = 3.8)
  
  # Assert
  expect_equal(actual_output, expected_output)
})
