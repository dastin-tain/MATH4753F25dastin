test_that("myncurve returns correct list components", {
  result <- myncurve(mu = 10, sigma = 5, a = 6)

  # Test 1: mu is correct
  expect_equal(result$mu, 10)

  # Test 2: sigma is correct
  expect_equal(result$sigma, 5)

  # Test 3: prob is computed correctly
  expected_prob <- round(pnorm(6, mean = 10, sd = 5), 4)
  expect_equal(result$prob, expected_prob)
})
