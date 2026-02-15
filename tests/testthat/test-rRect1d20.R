
test_that("d20 with eav returns consistent results with a seed", {
  set.seed(123)
  expected <- rRect1d20(12, eav = 5)

  set.seed(123)
  expect_equal(rRect1d20(12, eav = 5), expected)
})

test_that("outcomes are between eav and 20", {
  eav <- 5
  results <- replicate(200, rRect1d20(1, eav = eav))

  expect_true(all(results >= eav))
  expect_true(all(results <= 20L))
})

test_that("outcomes are between eav and 20", {
  eav <- sample(20, 1)
  results <- rRect1d20(200, eav = eav)
  expect_true(all(results >= eav))
  expect_true(all(results <= 20L))
})

test_that("d20 has the correct weighted distribution", {
  eav <- 5
  n <- 5000
  rolls <- replicate(n, rRect1d20(1, eav = eav))

  # The expected proportion for the eav is eav/20
  expected_p <- eav / 20
  actual_p <- sum(rolls == eav) / n

  # Allow for a 2% margin of error due to randomness
  expect_equal(actual_p, expected_p, tolerance = 0.02)
})


test_that("n = 0 returns an empty set", {
  expect_equal(rRect1d20(0, 1), integer())
})
test_that("length(n) > 1 , then length(n) is used for n", {
  len <- sample(1:10, 1)
  expect_length(rRect1d20(rep(0, len), 12), len)
})



#
# EXCEPTIONS
#
test_that("length(eav) > 1 throws exception", {
  expect_error(rRect1d20(1:20, 1:2L))
})
test_that("length(eav) < 1 throws exception", {
  expect_error(rRect1d20(1:20, numeric()))
})

test_that("eav = 0 throws exception", {
  expect_error(rRect1d20(1:20, 0))
})
test_that("eav = 21 throws exception", {
  expect_error(rRect1d20(1:20, 21))
})

test_that("eav = NULL throws exception", {
  expect_error(rRect1d20(1:20, NULL))
})
test_that("eav = NA throws exception", {
  expect_error(rRect1d20(5, NA))
})

