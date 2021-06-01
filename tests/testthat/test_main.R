test_that("Check if normalEM() is called when x is a vector", {
  skip_on_cran()
  skip_on_bioc()
  
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
  skip_on_cran()
  skip_on_bioc()
  
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
  skip_on_cran()
  skip_on_bioc()
  
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
  skip_on_cran()
  skip_on_bioc()
  
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
  skip_on_cran()
  skip_on_bioc()
  
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
  skip_on_cran()
  skip_on_bioc()
  
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
  skip_on_cran()
  skip_on_bioc()
  
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
  skip_on_cran()
  skip_on_bioc()
  
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
  skip_on_cran()
  skip_on_bioc()
  
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


## ==================== TESTING IF MIXFIT() RETURNS EXPECTED MODEL FITTING RESULTS =================

test_that("Check if mixfit() returns expected normal mixture results", {
  skip_on_cran()
  skip_on_bioc()
  
  # Arrange
  x = c(-0.429400536602601, 0.162010149716281, -3.17054746835854, 0.373705619911683,
        -0.80429729241256, 0.923022033500558, -1.81296118115276, 1.48477630658963,
        0.509796985059975, -1.19673682455782, -0.0383406596663621, 1.62703076926252,
        1.10663181681395, 1.2977549050602, -0.530487999126224, 0.586426384647559,
        -1.46830671121146, -0.862214632174845, -1.31043128283346, -1.15888895551618,
        -0.699645776132561, 0.0096628330180341, -2.02853975895703, -0.0694709147694598,
        0.877414694029549, -0.967976844745967, 1.11370723893191, -0.244643820108224,
        1.63384440842333, -0.789833336636973, -1.4621521685602, -0.434881033184263,
        -1.96204953269541, -0.00784395562823364, -1.85992617593592, -1.77637379686295,
        1.05585696137757, -0.0589836771931402, 0.386704628258097, 0.901125816036791,
        1.4257620504981, 0.40449178268071, -0.0258967091137805, 2.22446073468128,
        1.41422815573527, 0.662836208415823, 1.46776900904065, -0.503061978558018,
        1.08104257109407, 1.33654710593492, 0.579201339480091, 0.272925565639441,
        1.68610349913612, 1.9861095160581, 1.42454577082464, -0.436941621462757,
        0.186129780781515, 1.34975253037202, 1.98794209105315, 0.341088910553767,
        0.982016841884978, 0.655989909775022, 0.169651275996377, 2.07510480131931)
  
  # Act
  actual_output = mixfit(x, ncomp = 2)
  
  # Assert
  expect_equal(actual_output$pi, c(0.735983324861078, 0.264016675138922), tolerance = 1e-3)
  expect_equal(actual_output$mu, c(-0.239752814539843, 1.3579514588846), tolerance = 1e-3)
  expect_equal(actual_output$sd, c(1.08788231341068, 0.449605625221705), tolerance = 1e-3)
  expect_equal(actual_output$loglik, -99.3308270733505, tolerance = 1e-2)
  expect_equal(actual_output$aic, 208.661654146701, tolerance = 1e-2)
  expect_equal(actual_output$bic, 219.456069563499, tolerance = 1e-2)
})


test_that("Check if mixfit() returns equal variance for normal mixture when ev = TRUE", {
  skip_on_cran()
  skip_on_bioc()
  
  # Arrange
  x = c(-0.429400536602601, 0.162010149716281, -3.17054746835854, 0.373705619911683,
        -0.80429729241256, 0.923022033500558, -1.81296118115276, 1.48477630658963,
        0.509796985059975, -1.19673682455782, -0.0383406596663621, 1.62703076926252,
        1.10663181681395, 1.2977549050602, -0.530487999126224, 0.586426384647559,
        -1.46830671121146, -0.862214632174845, -1.31043128283346, -1.15888895551618,
        -0.699645776132561, 0.0096628330180341, -2.02853975895703, -0.0694709147694598,
        0.877414694029549, -0.967976844745967, 1.11370723893191, -0.244643820108224,
        1.63384440842333, -0.789833336636973, -1.4621521685602, -0.434881033184263,
        -1.96204953269541, -0.00784395562823364, -1.85992617593592, -1.77637379686295,
        1.05585696137757, -0.0589836771931402, 0.386704628258097, 0.901125816036791,
        1.4257620504981, 0.40449178268071, -0.0258967091137805, 2.22446073468128,
        1.41422815573527, 0.662836208415823, 1.46776900904065, -0.503061978558018,
        1.08104257109407, 1.33654710593492, 0.579201339480091, 0.272925565639441,
        1.68610349913612, 1.9861095160581, 1.42454577082464, -0.436941621462757,
        0.186129780781515, 1.34975253037202, 1.98794209105315, 0.341088910553767,
        0.982016841884978, 0.655989909775022, 0.169651275996377, 2.07510480131931)
  
  # Act
  actual_output = mixfit(x, ncomp = 2, ev = TRUE)
  
  # Assert
  expect_equal(actual_output$sd[1], actual_output$sd[2])
})


test_that("Check if mixfit() returns expected normal mixture results when x is a matrix", {
  skip_on_cran()
  skip_on_bioc()
  
  # Arrange
  x = structure(c(-6.34716872668756, -5.8129957820168, -5.27882283734603, 
                  -4.74464989267527, -4.21047694800451, -3.67630400333374, -3.14213105866298, 
                  -2.60795811399222, -2.07378516932145, -1.53961222465069, -1.00543927997993, 
                  -0.471266335309162, 0.0629066093616011, 0.597079554032364, 1.13125249870313, 
                  1.66542544337389, 2.19959838804465, 2.73377133271542, 3.26794427738618, 
                  3.80211722205694, 4.33629016672771, 4.87046311139847, 5.40463605606923, 
                  5.93880900074, 6.47298194541076, 7.00715489008152, 8.60967372409381, 
                  -5.8129957820168, -5.27882283734603, -4.74464989267527, -4.21047694800451, 
                  -3.67630400333374, -3.14213105866298, -2.60795811399222, -2.07378516932145, 
                  -1.53961222465069, -1.00543927997993, -0.471266335309162, 0.0629066093616011, 
                  0.597079554032364, 1.13125249870313, 1.66542544337389, 2.19959838804465, 
                  2.73377133271542, 3.26794427738618, 3.80211722205694, 4.33629016672771, 
                  4.87046311139847, 5.40463605606923, 5.93880900074, 6.47298194541076, 
                  7.00715489008152, 7.54132783475229, 9.14384666876458, 1, 4, 14, 
                  22, 72, 82, 83, 83, 53, 30, 32, 31, 35, 50, 64, 68, 65, 59, 36, 
                  37, 34, 20, 9, 9, 5, 1, 1), 
                .Dim = c(27L, 3L), 
                .Dimnames = list(NULL, c("a", "b", "freq")))
  
  # Act
  actual_output = mixfit(x, ncomp = 2)
  
  # Assert
  expect_equal(actual_output$pi, c(0.429159706460855, 0.570840293539145), tolerance = 1e-3)
  expect_equal(actual_output$mu, c(-2.9958961706014, 2.16414533267157), tolerance = 1e-3)
  expect_equal(actual_output$sd, c(0.987176784103023, 1.90335204760681), tolerance = 1e-3)
  expect_equal(actual_output$loglik, -3000.959021084, tolerance = 10)
  expect_equal(actual_output$aic, 6011.91804216799, tolerance = 10)
  expect_equal(actual_output$bic, 6036.4568185629, tolerance = 10)
})


test_that("Check if mixfit() returns expected normal mixture results when x is a matrix and ev = TRUE", {
  skip_on_cran()
  skip_on_bioc()
  
  # Arrange
  x = structure(c(-6.34716872668756, -5.8129957820168, -5.27882283734603, 
                  -4.74464989267527, -4.21047694800451, -3.67630400333374, -3.14213105866298, 
                  -2.60795811399222, -2.07378516932145, -1.53961222465069, -1.00543927997993, 
                  -0.471266335309162, 0.0629066093616011, 0.597079554032364, 1.13125249870313, 
                  1.66542544337389, 2.19959838804465, 2.73377133271542, 3.26794427738618, 
                  3.80211722205694, 4.33629016672771, 4.87046311139847, 5.40463605606923, 
                  5.93880900074, 6.47298194541076, 7.00715489008152, 8.60967372409381, 
                  -5.8129957820168, -5.27882283734603, -4.74464989267527, -4.21047694800451, 
                  -3.67630400333374, -3.14213105866298, -2.60795811399222, -2.07378516932145, 
                  -1.53961222465069, -1.00543927997993, -0.471266335309162, 0.0629066093616011, 
                  0.597079554032364, 1.13125249870313, 1.66542544337389, 2.19959838804465, 
                  2.73377133271542, 3.26794427738618, 3.80211722205694, 4.33629016672771, 
                  4.87046311139847, 5.40463605606923, 5.93880900074, 6.47298194541076, 
                  7.00715489008152, 7.54132783475229, 9.14384666876458, 1, 4, 14, 
                  22, 72, 82, 83, 83, 53, 30, 32, 31, 35, 50, 64, 68, 65, 59, 36, 
                  37, 34, 20, 9, 9, 5, 1, 1), 
                .Dim = c(27L, 3L), 
                .Dimnames = list(NULL, c("a", "b", "freq")))
  
  # Act
  actual_output = mixfit(x, ncomp = 2, ev = TRUE)
  
  # Assert
  expect_equal(actual_output$pi, c(0.508269167565811, 0.491730832434189), tolerance = 1e-3)
  expect_equal(actual_output$mu, c(-2.62290318710918, 2.60878266400357), tolerance = 1e-3)
  expect_equal(actual_output$sd, c(1.47257539302977, 1.47257539302977), tolerance = 1e-3)
  expect_equal(actual_output$loglik, -3034.76838334493, tolerance = 10)
  expect_equal(actual_output$aic, 6077.53676668985, tolerance = 10)
  expect_equal(actual_output$bic, 6097.16778780578, tolerance = 10)
})


test_that("Check if mixfit() returns expected gamma mixture results", {
  skip_on_cran()
  skip_on_bioc()
  
  # Arrange
  # set.seed(4)
  # x = rmixgamma(100, c(0.3, 0.7), c(1.5, 4), c(1, 1))
  x = c(0.737282887057245, 0.669151738903973, 3.0550205312093, 1.85373390718685, 
        0.310312170728521, 1.06932380256818, 3.25991987241765, 1.71987164086492, 
        1.18057110555297, 1.5289469726302, 1.12719903200373, 1.23348555940815, 
        0.62472365539016, 1.12800807100084, 1.45695103796523, 4.67259578834858, 
        0.930476646831949, 2.0653605356643, 0.997908362667444, 2.5167447778597, 
        1.3059345604871, 2.27881102512773, 0.596320657212758, 1.1997422024859, 
        3.4557580223342, 1.48127672539276, 0.84283475101908, 0.791961616875531, 
        0.392387446735569, 1.33225834324506, 5.25156829801265, 2.39163077739572, 
        5.45549929449615, 2.83279489493364, 3.5140808537938, 3.73881161016938, 
        3.24924484448778, 3.95555173039128, 4.06182455804466, 3.12838735112444, 
        3.30989724629822, 3.85773163988959, 3.72330204398945, 2.56493844211852, 
        4.99350265725243, 2.6906547131488, 3.09278730685162, 3.65031538720081, 
        3.03847444494887, 4.41018576114012, 3.70825754977278, 4.3220907876966, 
        5.63527958761936, 3.72884644598393, 4.61792395166981, 3.51981443099945, 
        5.81172804843568, 4.94899265441581, 3.31685068939921, 5.24517201214943, 
        3.6670892385508, 2.57166501798731, 2.92518547293442, 4.15876331163962, 
        3.88318922442146, 7.12085799531385, 2.71948790985402, 5.20541204624909, 
        3.23311049582059, 4.5816929840271, 3.82882835841046, 3.18899104124271, 
        4.88684853075119, 3.60664941203729, 4.72058495742065, 3.53228113720234, 
        3.77159341813681, 2.31524759268227, 5.55756448431181, 4.67686099776576, 
        2.86921282497598, 4.30403315683404, 4.25421738794564, 3.34913522519378, 
        4.18134678695866, 3.76358553029534, 4.92473203107161, 3.03712056368614, 
        5.7468296984434, 3.20236357393419, 3.5367274818947, 5.93868630048978, 
        3.5500395383361, 3.29611083566205, 3.00394000681805, 4.1471107700129, 
        7.4834866698749, 5.11964278823513, 2.76437801807261, 2.98372265539294)
  
  # Act
  actual_output = mixfit(x, ncomp = 2, family = 'gamma')
  
  # Assert
  expect_equal(actual_output$pi, c(0.227150017974776, 0.772849982025224), tolerance = 1e-3)
  expect_equal(actual_output$mu, c(1.08085190158171, 3.88998891164942), tolerance = 1e-3)
  expect_equal(actual_output$sd, c(0.464536909424077, 1.0798220486424), tolerance = 1e-3)
  expect_equal(actual_output$loglik, -175.219817276001, tolerance = 1e-1)
  expect_equal(actual_output$aic, 360.439634552002, tolerance = 1e-1)
  expect_equal(actual_output$bic, 373.465485481943, tolerance = 1e-1)
})


test_that("Check if mixfit() returns expected gamma mixture results when x is a matrix", {
  skip_on_cran()
  skip_on_bioc()
  
  # Arrange
  # set.seed(4)
  # x = rmixgamma(300, c(0.3, 0.7), c(1.5, 4), c(1, 1))
  # x = bin(x, seq(min(x), max(x), length = 30))
  x = structure(c(0.122727895127436, 0.416884925197281, 0.711041955267126, 
                  1.00519898533697, 1.29935601540681, 1.59351304547666, 1.8876700755465, 
                  2.18182710561635, 2.47598413568619, 2.77014116575604, 3.06429819582588, 
                  3.35845522589573, 3.65261225596557, 3.94676928603542, 4.24092631610526, 
                  4.53508334617511, 4.82924037624495, 5.1233974063148, 5.41755443638464, 
                  5.71171146645449, 6.00586849652433, 6.30002552659417, 6.59418255666402, 
                  7.47665364687355, 8.35912473708309, 0.416884925197281, 0.711041955267126, 
                  1.00519898533697, 1.29935601540681, 1.59351304547666, 1.8876700755465, 
                  2.18182710561635, 2.47598413568619, 2.77014116575604, 3.06429819582588, 
                  3.35845522589573, 3.65261225596557, 3.94676928603542, 4.24092631610526, 
                  4.53508334617511, 4.82924037624495, 5.1233974063148, 5.41755443638464, 
                  5.71171146645449, 6.00586849652433, 6.30002552659417, 6.59418255666402, 
                  6.88833958673386, 7.7708106769434, 8.65328176715293, 7, 13, 11, 
                  13, 9, 4, 10, 7, 15, 24, 22, 30, 19, 23, 25, 14, 19, 10, 8, 5, 
                  4, 3, 3, 1, 1), 
                .Dim = c(25L, 3L), 
                .Dimnames = list(NULL, c("a", "b", "freq")))
  
  # Act
  actual_output = mixfit(x, ncomp = 2, family = 'gamma')
  
  # Assert
  expect_equal(actual_output$pi, c(0.195465297199426, 0.804534702800574), tolerance = 1e-3)
  expect_equal(actual_output$mu, c(1.00638758218284, 3.97514719426555), tolerance = 1e-3)
  expect_equal(actual_output$sd, c(0.519290775183046, 1.12832735183624), tolerance = 1e-3)
  expect_equal(actual_output$loglik, -904.713654418661, tolerance = 1e-1)
  expect_equal(actual_output$aic, 1819.42730883732, tolerance = 1e-1)
  expect_equal(actual_output$bic, 1837.9462212106, tolerance = 1e-1)
})


test_that("Check if mixfit() returns expected log-normal mixture results", {
  skip_on_cran()
  skip_on_bioc()
  
  # Arrange
  # set.seed(4)
  # x = rmixlnorm(100, c(0.3, 0.7), c(1.5, 4), c(1, 1))
  x = c(0.898196435763888, 2.14254770604214, 1.79141752039204, 3.36506280072122, 
        1.89568740057817, 0.57387178760261, 1.09674994419916, 3.94193007514335, 
        3.66598906882752, 1.75978821680811, 1.2600293879579, 1.57442463541592, 
        1.21437729229504, 1.27434691378181, 1.38278708093604, 2.52964420615004, 
        1.21506463739157, 1.17437839058072, 1.05097787330203, 3.17706487648141, 
        1.37955603253353, 2.75811930462128, 2.72591934794335, 1.78807083057065, 
        1.0512971436722, 2.67292865814177, 2.16697469530029, 0.710947673161292, 
        2.6475965986527, 1.36979881667418, 5.02783564397152, 3.22289381003995, 
        2.69402522575972, 4.79709498338257, 3.51268570704281, 3.66926046798956, 
        4.88405511918515, 3.46000090712957, 3.31681723292111, 5.40233300341494, 
        4.05795769198238, 5.3346608544319, 2.56087651083795, 3.17033244825801, 
        3.13837089764374, 3.97617145470478, 3.53773957926234, 4.63771701005155, 
        2.49285697596919, 3.29548966197364, 3.32812037224874, 3.80522415007389, 
        4.31994155946829, 6.30446820826738, 3.35029273573101, 3.38699585650945, 
        4.6059245754361, 3.73465096138386, 5.40924040424422, 2.98288089357345, 
        5.04335650581114, 2.80881534359402, 6.45016916157368, 4.00815591531419, 
        3.66539301210144, 3.51888755555968, 4.83063833659678, 4.41733135260102, 
        3.72032502345766, 4.03518271544721, 3.44319999051243, 3.06448505519289, 
        4.05694058921042, 4.6352398905104, 3.54306986014888, 4.11430075724641, 
        3.29372208067926, 3.18926845371748, 3.83148783636109, 5.32733128492938, 
        3.68125591518567, 3.36850098168321, 2.70163904243493, 3.00927893639194, 
        2.81310432552496, 3.15688876617206, 2.93759651569969, 4.24939013757262, 
        3.69246395579208, 2.83316869044953, 3.18832237405461, 4.03558564356887, 
        4.51478668409328, 4.59683978113662, 3.83587326599106, 6.88782549132984, 
        3.36616246421881, 4.92557805044035, 3.62424880312133, 4.59328037080621)
  
  # Act
  actual_output = mixfit(x, ncomp = 2, family = 'lnorm')
  
  # Assert
  expect_equal(actual_output$pi, c(0.200304433335958, 0.799695566664042), tolerance = 1e-3)
  expect_equal(actual_output$mu, c(1.3650418991622, 3.79192643446539), tolerance = 1e-3)
  expect_equal(actual_output$sd, c(0.483106684980295, 0.930709134027962), tolerance = 1e-3)
  expect_equal(actual_output$loglik, -160.272435981767, tolerance = 1e-1)
  expect_equal(actual_output$aic, 330.544871963535, tolerance = 1e-1)
  expect_equal(actual_output$bic, 343.570722893475, tolerance = 1e-1)
})


test_that("Check if mixfit() returns expected log-normal mixture results when x is a matrix", {
  skip_on_cran()
  skip_on_bioc()
  
  # Arrange
  # set.seed(4)
  # x = rmixlnorm(300, c(0.3, 0.7), c(1.5, 4), c(1, 1))
  # x = bin(x, seq(min(x), max(x), length = 30))
  x = structure(c(0.419649516977373, 0.653893407159821, 0.888137297342269, 
                  1.12238118752472, 1.35662507770716, 1.59086896788961, 1.82511285807206, 
                  2.05935674825451, 2.29360063843696, 2.5278445286194, 2.76208841880185, 
                  2.9963323089843, 3.23057619916675, 3.4648200893492, 3.69906397953164, 
                  3.93330786971409, 4.16755175989654, 4.40179565007899, 4.63603954026143, 
                  4.87028343044388, 5.10452732062633, 5.33877121080878, 5.57301510099123, 
                  5.80725899117367, 6.04150288135612, 6.27574677153857, 6.74423455190346, 
                  6.97847844208591, 0.653893407159821, 0.888137297342269, 1.12238118752472, 
                  1.35662507770716, 1.59086896788961, 1.82511285807206, 2.05935674825451, 
                  2.29360063843696, 2.5278445286194, 2.76208841880185, 2.9963323089843, 
                  3.23057619916675, 3.4648200893492, 3.69906397953164, 3.93330786971409, 
                  4.16755175989654, 4.40179565007899, 4.63603954026143, 4.87028343044388, 
                  5.10452732062633, 5.33877121080878, 5.57301510099123, 5.80725899117367, 
                  6.04150288135612, 6.27574677153857, 6.50999066172102, 6.97847844208591, 
                  7.21272233226836, 6, 11, 13, 11, 8, 5, 5, 9, 6, 17, 16, 27, 22, 
                  23, 22, 15, 20, 20, 7, 11, 9, 2, 5, 4, 2, 1, 2, 1), 
                .Dim = c(28L, 3L), 
                .Dimnames = list(NULL, c("a", "b", "freq")))
  
  # Act
  actual_output = mixfit(x, ncomp = 2, family = 'lnorm')
  
  # Assert
  expect_equal(actual_output$pi, c(0.198366229468149, 0.801633770531851), tolerance = 1e-3)
  expect_equal(actual_output$mu, c(1.19850343316745, 3.82984927949325), tolerance = 1e-3)
  expect_equal(actual_output$sd, c(0.475106688135424, 1.00709523668601), tolerance = 1e-3)
  expect_equal(actual_output$loglik, -934.771755521696, tolerance = 10)
  expect_equal(actual_output$aic, 1879.54351104339, tolerance = 10)
  expect_equal(actual_output$bic, 1898.06242341667, tolerance = 10)
})


test_that("Check if mixfit() returns expected weilbull mixture results", {
  skip_on_cran()
  skip_on_bioc()
  
  # Arrange
  # set.seed(4)
  # x = rmixweibull(100, c(0.3, 0.7), c(1.5, 4), c(1, 1))
  x = c(1.90105455031286, 1.95785802821754, 0.603839774491944, 2.01929641607348, 
        0.80488574137674, 0.375417349372989, 0.249576417132426, 3.09775925927453, 
        0.737509786527813, 1.92762725568001, 2.85347218843502, 0.2330354465704, 
        1.53412955672173, 1.43003205418732, 0.172129719182638, 1.11903515434795, 
        0.204989416132925, 0.721780809475411, 0.826832498354656, 0.0428910583085811, 
        1.30221691794183, 1.34225577358315, 0.971854982979589, 0.563596115152414, 
        1.36197640413656, 0.537654879208504, 1.28420089846739, 1.24563149010597, 
        1.15793997605212, 2.10154881484782, 2.78754242263843, 3.62270808701963, 
        4.08601915194431, 2.00388033160759, 4.14356982202868, 3.71498837542732, 
        4.32885330141263, 6.27015771191157, 2.37869359038574, 4.73610057566523, 
        3.86868421984393, 4.93682938621507, 2.63155644997206, 5.35836280762905, 
        2.65269910907203, 2.71076148849277, 3.41266188277189, 3.87077351754441, 
        4.32835131151612, 3.33805538740722, 2.68807646690274, 3.10602564213177, 
        3.06733400334731, 4.24365846054677, 4.95180993759005, 4.96229792089678, 
        2.70525064263611, 3.3468206567938, 3.88094629860142, 5.43721283456872, 
        2.91155435622625, 2.57014220100636, 4.78839326143229, 3.69677124779701, 
        5.45074636840839, 4.00199008021136, 3.12065221214115, 1.99192725652029, 
        4.44939369901385, 3.68887996275927, 4.27251984041964, 4.44192634574972, 
        3.04076024655676, 3.51765163269958, 4.50988068397355, 4.19171136675368, 
        4.67681283350997, 5.12055571959566, 2.59220453610179, 3.45798041816919, 
        3.85151730846795, 2.54729893246961, 2.64805171341415, 5.523759044877, 
        5.6278560603418, 1.61934917709464, 4.85157820804559, 2.45637701671218, 
        4.89026607430497, 5.14806521427685, 3.93799558062917, 4.64615107519535, 
        4.42080286639809, 5.35942693280845, 3.27045462157038, 4.18357989670015, 
        5.72018844710079, 3.48261603873742, 4.70200279094218, 3.69375891619698)
  
  # Act
  actual_output = mixfit(x, ncomp = 2, family = 'weibull')
  
  # Assert
  expect_equal(actual_output$pi, c(0.337594356885263, 0.662405643114737), tolerance = 1e-3)
  expect_equal(actual_output$mu, c(1.45597526792847, 3.90051372223834), tolerance = 1e-3)
  expect_equal(actual_output$sd, c(1.12465816614896, 1.05992237356596), tolerance = 1e-3)
  expect_equal(actual_output$loglik, -178.000820893679, tolerance = 1e-1)
  expect_equal(actual_output$aic, 366.001641787358, tolerance = 1e-1)
  expect_equal(actual_output$bic, 379.027492717298, tolerance = 1e-1)
})


test_that("Check if mixfit() returns expected weilbull mixture results when x is a matrix", {
  skip_on_cran()
  skip_on_bioc()
  
  # Arrange
  # set.seed(4)
  # x = rmixweibull(100, c(0.3, 0.7), c(1.5, 4), c(1, 1))
  # x = bin(x, seq(min(x), max(x), length = 30))
  x = structure(c(0.0428910583085811, 0.257624391191443, 0.472357724074304, 
                  0.687091056957166, 0.901824389840028, 1.11655772272289, 1.33129105560575, 
                  1.54602438848861, 1.76075772137147, 1.97549105425434, 2.1902243871372, 
                  2.40495772002006, 2.61969105290292, 2.83442438578578, 3.04915771866864, 
                  3.26389105155151, 3.47862438443437, 3.69335771731723, 3.90809105020009, 
                  4.12282438308295, 4.33755771596581, 4.55229104884867, 4.76702438173154, 
                  4.9817577146144, 5.19649104749726, 5.41122438038012, 5.62595771326298, 
                  6.05542437902871, 0.257624391191443, 0.472357724074304, 0.687091056957166, 
                  0.901824389840028, 1.11655772272289, 1.33129105560575, 1.54602438848861, 
                  1.76075772137147, 1.97549105425434, 2.1902243871372, 2.40495772002006, 
                  2.61969105290292, 2.83442438578578, 3.04915771866864, 3.26389105155151, 
                  3.47862438443437, 3.69335771731723, 3.90809105020009, 4.12282438308295, 
                  4.33755771596581, 4.55229104884867, 4.76702438173154, 4.9817577146144, 
                  5.19649104749726, 5.41122438038012, 5.62595771326298, 5.84069104614585, 
                  6.27015771191157, 5, 1, 3, 4, 1, 5, 4, 1, 3, 4, 1, 4, 7, 3, 4, 
                  5, 4, 7, 3, 7, 4, 4, 6, 2, 2, 3, 2, 1), 
                .Dim = c(28L, 3L), 
                .Dimnames = list(NULL, c("a", "b", "freq")))
  
  # Act
  actual_output = mixfit(x, ncomp = 2, family = 'weibull')
  
  # Assert
  expect_equal(actual_output$pi, c(0.331197820185521, 0.668802179814479), tolerance = 1e-3)
  expect_equal(actual_output$mu, c(1.42592195387772, 3.89438483122951), tolerance = 1e-3)
  expect_equal(actual_output$sd, c(1.0894890106431, 1.0489337243565), tolerance = 1e-3)
  expect_equal(actual_output$loglik, -331.438875275047, tolerance = 1e-1)
  expect_equal(actual_output$aic, 672.877750550095, tolerance = 1e-1)
  expect_equal(actual_output$bic, 685.903601480035, tolerance = 1e-1)
})


## ==================== TESTING IF MIXFIT() RETURNS EXPECTED RESULTS FOR INCOMPLETE INPUTS =================


test_that("Check if error is thrown when none of ncomp, pi, mu, sd is provided", {
  skip_on_cran()
  skip_on_bioc()
  
  # Arrange
  m = mockery::mock(0, cycle = TRUE)
  mockery::stub(mixfit, 'CheckData', m)
  
  # Assert
  x = c(1, 2, 3)
  expect_error(mixfit(x), "provide 'ncomp' or all of 'pi', 'mu' and 'sd'.")
  expect_error(mixfit(x, family = 'gamma'), "provide 'ncomp' or all of 'pi', 'mu' and 'sd'.")
  expect_error(mixfit(x, family = 'lnorm'), "provide 'ncomp' or all of 'pi', 'mu' and 'sd'.")
  expect_error(mixfit(x, family = 'weibull'), "provide 'ncomp' or all of 'pi', 'mu' and 'sd'.")
  
  x = matrix(1:6, ncol = 3)
  expect_error(mixfit(x), "provide 'ncomp' or all of 'pi', 'mu' and 'sd'.")
  expect_error(mixfit(x, family = 'gamma'), "provide 'ncomp' or all of 'pi', 'mu' and 'sd'.")
  expect_error(mixfit(x, family = 'lnorm'), "provide 'ncomp' or all of 'pi', 'mu' and 'sd'.")
  expect_error(mixfit(x, family = 'weibull'), "provide 'ncomp' or all of 'pi', 'mu' and 'sd'.")
  
  mockery::expect_called(m, 8)
})


test_that("Check if error is throw when pi, mu, and sd are of different lengths", {
  skip_on_cran()
  skip_on_bioc()
  
  # Arrange
  m = mockery::mock(0, cycle = TRUE)
  mockery::stub(mixfit, 'CheckData', m)

  # Assert
  # set.seed(3)
  # x = rnorm(100)
  x = c(-0.961933415919883, -0.292525722878467, 0.258788216241251, 
        -1.15213188591513, 0.195782826286375, 0.0301239446016315, 0.0854177316122717, 
        1.11661021271527, -1.21885741557799, 1.26736872208989, -0.744781596135141, 
        -1.13121857083574, -0.716358490032977, 0.252652369646215, 0.152045706655596, 
        -0.30765642967842, -0.953017330908122, -0.64824281144847, 1.22431362428059, 
        0.19981160798297, -0.578483721859972, -0.942300733477539, -0.203728179661995, 
        -1.66647484003016, -0.484455109150991, -0.741072660721595, 1.16061577924134, 
        1.01206712493423, -0.0720784740865598, -1.13678229809932, 0.90062472898267, 
        0.851770447092221, 0.72771517415448, 0.73650214568861, -0.352129616945692, 
        0.705515513485513, 1.30035798873208, 0.0382520141442711, -0.979283769996278, 
        0.793761230872534, 0.786506872009449, -0.3104631309593, 1.6988848455591, 
        -0.794593708549281, 0.348437716180028, -2.26540107351778, -0.162205279028246, 
        1.13086499145889, -0.455545976260457, -0.899166315538872, 0.726838901731248, 
        -0.809440901865235, 0.267085115955233, -1.73726371053359, -1.41142513580887, 
        -0.453551226778487, -1.03549127536806, 1.36214289325928, 0.917456736975022, 
        -0.785142161076832, 0.573518173147265, 0.918196207737772, 0.256287272985725, 
        0.351966555937571, 1.1743373570956, -0.480846375289203, -0.418829722135601, 
        0.955112803220351, -1.28900661094787, 0.186197433075043, -0.0313255019472277, 
        0.46709730984657, 1.02419767420456, 0.267358452234395, 0.2318261028602, 
        0.747592464522409, 1.21706851052422, 0.383358345168268, -0.988052821592123, 
        -0.156852910196835, 1.73553521624809, -0.352298305500082, 0.68864004412573, 
        1.22440609580274, 0.794296303303488, -0.00640239842172925, 0.219150635166991, 
        -0.886463751005381, 0.439760291371926, -0.886389750665543, -0.853818454354972, 
        -0.989994330748501, -0.650877736923895, 1.05394666049401, -0.39087803343083, 
        -0.0705863936100801, -0.462050809497045, 0.540908266990262, 0.931634970926501, 
        -0.209274345208391)
  
  expect_error(mixfit(x, pi = c(0.3, 0.7), mu = c(1, 2, 3)), 
               "the length of 'pi', 'mu' and 'sd' should be the same.")
  expect_error(mixfit(x, pi = c(0.3, 0.7), sd = c(1, 1, 2)), 
               "the length of 'pi', 'mu' and 'sd' should be the same.")
  expect_error(mixfit(x, mu = c(2, 4), sd = c(1, 1, 2)), 
               "the length of 'pi', 'mu' and 'sd' should be the same.")
  
  expect_error(mixfit(x, family = 'gamma', pi = c(0.3, 0.7), mu = c(1, 2, 3)), 
               "the length of 'pi', 'mu' and 'sd' should be the same.")
  expect_error(mixfit(x, family = 'gamma', pi = c(0.3, 0.7), sd = c(1, 1, 2)), 
               "the length of 'pi', 'mu' and 'sd' should be the same.")
  expect_error(mixfit(x, family = 'gamma', mu = c(2, 4), sd = c(1, 1, 2)), 
               "the length of 'pi', 'mu' and 'sd' should be the same.")
  
  expect_error(mixfit(x, family = 'lnorm', pi = c(0.3, 0.7), mu = c(1, 2, 3)), 
               "the length of 'pi', 'mu' and 'sd' should be the same.")
  expect_error(mixfit(x, family = 'lnorm', pi = c(0.3, 0.7), sd = c(1, 1, 2)), 
               "the length of 'pi', 'mu' and 'sd' should be the same.")
  expect_error(mixfit(x, family = 'lnorm', mu = c(2, 4), sd = c(1, 1, 2)), 
               "the length of 'pi', 'mu' and 'sd' should be the same.")
  
  expect_error(mixfit(x, family = 'weibull', pi = c(0.3, 0.7), mu = c(1, 2, 3)), 
               "the length of 'pi', 'mu' and 'sd' should be the same.")
  expect_error(mixfit(x, family = 'weibull', pi = c(0.3, 0.7), sd = c(1, 1, 2)), 
               "the length of 'pi', 'mu' and 'sd' should be the same.")
  expect_error(mixfit(x, family = 'weibull', mu = c(2, 4), sd = c(1, 1, 2)), 
               "the length of 'pi', 'mu' and 'sd' should be the same.")
  
  # Assert
  # set.seed(3)
  # x = rnorm(100)
  # x = bin(x, seq(min(x), max(x), length = 30))
  x = structure(c(-2.26540107351778, -1.85151111250752, -1.71354779217076, 
                  -1.43762115149726, -1.2996578311605, -1.16169451082375, -1.02373119048699, 
                  -0.88576787015024, -0.747804549813486, -0.609841229476732, -0.471877909139978, 
                  -0.333914588803224, -0.19595126846647, -0.0579879481297159, 0.0799753722070382, 
                  0.217938692543792, 0.355902012880546, 0.4938653332173, 0.631828653554054, 
                  0.769791973890809, 0.907755294227563, 1.04571861456432, 1.18368193490107, 
                  1.32164525523782, 1.59757189591133, -2.12743775318103, -1.71354779217076, 
                  -1.57558447183401, -1.2996578311605, -1.16169451082375, -1.02373119048699, 
                  -0.88576787015024, -0.747804549813486, -0.609841229476732, -0.471877909139978, 
                  -0.333914588803224, -0.19595126846647, -0.0579879481297159, 0.0799753722070382, 
                  0.217938692543792, 0.355902012880546, 0.4938653332173, 0.631828653554054, 
                  0.769791973890809, 0.907755294227563, 1.04571861456432, 1.18368193490107, 
                  1.32164525523782, 1.45960857557458, 1.73553521624809, 1, 1, 1, 
                  1, 2, 4, 9, 4, 5, 3, 7, 5, 4, 4, 5, 9, 3, 2, 6, 5, 6, 5, 5, 1, 
                  2), 
                .Dim = c(25L, 3L), 
                .Dimnames = list(NULL, c("a", "b", "freq")))
  
  expect_error(mixfit(x, pi = c(0.3, 0.7), mu = c(1, 2, 3)), 
               "the length of 'pi', 'mu' and 'sd' should be the same.")
  expect_error(mixfit(x, pi = c(0.3, 0.7), sd = c(1, 1, 2)), 
               "the length of 'pi', 'mu' and 'sd' should be the same.")
  expect_error(mixfit(x, mu = c(2, 4), sd = c(1, 1, 2)), 
               "the length of 'pi', 'mu' and 'sd' should be the same.")
  
  expect_error(mixfit(x, family = 'gamma', pi = c(0.3, 0.7), mu = c(1, 2, 3)), 
               "the length of 'pi', 'mu' and 'sd' should be the same.")
  expect_error(mixfit(x, family = 'gamma', pi = c(0.3, 0.7), sd = c(1, 1, 2)), 
               "the length of 'pi', 'mu' and 'sd' should be the same.")
  expect_error(mixfit(x, family = 'gamma', mu = c(2, 4), sd = c(1, 1, 2)), 
               "the length of 'pi', 'mu' and 'sd' should be the same.")
  
  expect_error(mixfit(x, family = 'lnorm', pi = c(0.3, 0.7), mu = c(1, 2, 3)), 
               "the length of 'pi', 'mu' and 'sd' should be the same.")
  expect_error(mixfit(x, family = 'lnorm', pi = c(0.3, 0.7), sd = c(1, 1, 2)), 
               "the length of 'pi', 'mu' and 'sd' should be the same.")
  expect_error(mixfit(x, family = 'lnorm', mu = c(2, 4), sd = c(1, 1, 2)), 
               "the length of 'pi', 'mu' and 'sd' should be the same.")
  
  expect_error(mixfit(x, family = 'weibull', pi = c(0.3, 0.7), mu = c(1, 2, 3)), 
               "the length of 'pi', 'mu' and 'sd' should be the same.")
  expect_error(mixfit(x, family = 'weibull', pi = c(0.3, 0.7), sd = c(1, 1, 2)), 
               "the length of 'pi', 'mu' and 'sd' should be the same.")
  expect_error(mixfit(x, family = 'weibull', mu = c(2, 4), sd = c(1, 1, 2)), 
               "the length of 'pi', 'mu' and 'sd' should be the same.")
})



