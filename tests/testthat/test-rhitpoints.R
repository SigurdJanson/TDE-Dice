# Detect if coverage analysis is running
is_coverage <- identical(Sys.getenv("R_COVR"), "true")
# Set iterations based on context
iterations <- if (is_coverage) 1L else 100000L


test_that("function returns consistent results with consistent seed", {
  eav <- sample(1:20, 1)
  diceCount <- sample(1:5, 1)
  weapon <- c(Count = diceCount, Faces = sample(2:20, 1), Mod = sample(-diceCount:5, 1))
  #-cat("DEBUG:", weapon)

  set.seed(123)
  expected <- rhitpoints(12, eav, weapon)

  set.seed(123)
  expect_equal(rhitpoints(12, eav, weapon), expected)
})


test_that("outcomes are between 0 and 12", {
  eav <- sample(1:20, 1)
  weapon <- c(Count = 1, Faces = 6, Mod = 0)
  results <- replicate(200, rhitpoints(1, eav, weapon))

  expect_true(all(results >= 0))
  expect_true(all(results <= 12))
})


test_that("outcomes are between 3 and 26", {
  eav <- sample(20, 1)
  weapon <- c(Count = 2, Faces = 6, Mod = 1)

  results <- replicate(200, rhitpoints(1, eav, weapon))

  expect_true(all(results >= 3 | results == 0))
  expect_true(all(results <= 26))
})


test_that("d20 has the correct weighted distribution", {
  eav <- 5
  n <- 5000
  weapon <- c(Count = sample(1:5, 1), Faces = sample(2:20, 1), Mod = sample(-5:5, 1))
  rolls <- replicate(n, rhitpoints(1, eav, weapon))
  actualHP <- sum(rolls > 0) / n
  actual0 <- sum(rolls == 0) / n

  # The expected proportion
  expectedSuccess <- eav / 20
  expectedFailure <- 1 - expectedSuccess

  # Allow for a 2% margin of error due to randomness
  expect_equal(actualHP, expectedSuccess, tolerance = 0.02)
  expect_equal(actual0, expectedFailure, tolerance = 0.02)
})


#
# EDGE CASES
#
test_that("n = 0 returns an empty set", {
  weapon <- c(Count = sample(1:5, 1), Faces = sample(2:20, 1), Mod = sample(-5:5, 1))
  expect_equal(rhitpoints(0, 12, weapon), integer())
})
test_that("n = numeric(0) returns an empty set", {
  weapon <- c(Count = 1, Faces = 6, Mod = 1)
  expect_equal(rhitpoints(numeric(0), 12, weapon), integer())
})
test_that("n < 0 throws an exception", {
  weapon <- c(Count = 1, Faces = 6, Mod = 1)
  expect_error(rhitpoints(-1, 12, weapon))
})
test_that("length(n) > 1 , then length(n) is used for n", {
  weapon <- c(Count = sample(1:5, 1), Faces = sample(2:20, 1), Mod = sample(-5:5, 1))
  len <- sample(2:10, 1)

  expect_length(rhitpoints(rep(0, len), 11, weapon), len)
})
test_that("eav = 21 is valid and equal to eav=20", {
  eav <- 21
  Count <- sample(1:5, 1)
  weapon <- c(Count = Count, Faces = sample(2:20, 1), Mod = sample(-Count:5, 1))

  set.seed(698)
  expect_no_error(result <- rhitpoints(1:20, eav, weapon))
  set.seed(698)
  expect_equal(result, rhitpoints(1:20, 20L, weapon))
})


#
# EXCEPTIONS
#
test_that("length(eav) > 1 throws exception", {
  eav <- 10:11
  weapon <- c(Count = sample(1:5, 1), Faces = sample(2:20, 1), Mod = sample(-5:5, 1))
  expect_error(rhitpoints(1, eav, weapon))
})
test_that("length(eav) < 1 throws exception", {
  eav <- numeric()
  weapon <- c(Count = sample(1:5, 1), Faces = sample(2:20, 1), Mod = sample(-5:5, 1))
  expect_error(rhitpoints(1:20, eav, weapon))
})

test_that("eav = 0 throws exception", {
  eav <- 0
  Count <- sample(1:5, 1)
  weapon <- c(Count = Count, Faces = sample(2:20, 1), Mod = sample(-Count:5, 1))
  expect_error(rhitpoints(1:20, 0))
})

test_that("eav = NULL throws exception", {
  expect_error(rhitpoints(1:20, NULL, weapon))
})
test_that("eav = NA throws exception", {
  expect_error(rhitpoints(5, NA, weapon))
})


#
# TEST CASES FROM test-dhitpoints.R
#
test_that("random hit points match the AT4 1W2+1 distribution", {
  # iterations <- 100000

  skill <- 4
  weapon <- c(Count = 1, Faces = 2, Mod = 1)

  set.seed(123)
  rolls <- rhitpoints(iterations, skill, weapon)
  actual <- tabulate(rolls + 1L, nbins = 7L) / iterations
  expected <- dhitpoints(0:6, skill, weapon)

  expect_equal(actual, unname(expected), tolerance = 0.003)
})

test_that("random hit points match the AT10 1W3+0 distribution", {
  iterations <- 100000*3.5

  skill <- 10
  weapon <- c(Count = 1, Faces = 3, Mod = 0)

  set.seed(123)
  rolls <- rhitpoints(iterations, skill, weapon)
  actual <- tabulate(rolls + 1L, nbins = 7L) / iterations
  expected <- dhitpoints(0:6, skill, weapon)

  expect_equal(actual, unname(expected), tolerance = 0.003)
})

test_that("random hit points match the AT10 1W6+0 distribution", {
  iterations <- iterations*3.5

  skill <- 10
  weapon <- c(Count = 1, Faces = 6, Mod = 0)

  set.seed(123)
  rolls <- rhitpoints(iterations, skill, weapon)
  actual <- tabulate(rolls + 1L, nbins = 13L) / iterations
  expected <- dhitpoints(0:12, skill, weapon)

  expect_equal(actual, unname(expected), tolerance = 0.003)
})

test_that("random hit points match the AT15 1W6+0 distribution", {
  iterations <- iterations*4

  skill <- 15
  weapon <- c(Count = 1, Faces = 6, Mod = 0)

  set.seed(123)
  rolls <- rhitpoints(iterations, skill, weapon)
  actual <- tabulate(rolls + 1L, nbins = 13L) / iterations
  expected <- dhitpoints(0:12, skill, weapon)

  expect_equal(actual, unname(expected), tolerance = 0.003)
})

test_that("random hit points respect 1W6 modifiers", {
  iterations <- iterations*4

  skill <- 10

  set.seed(123)
  rolls <- rhitpoints(iterations, skill, c(Count = 1, Faces = 6, Mod = 1))
  actual <- tabulate(rolls + 1L, nbins = 15L) / iterations
  expected <- dhitpoints(0:14, skill, c(Count = 1, Faces = 6, Mod = 1))
  expect_equal(actual, unname(expected), tolerance = 0.003)

  set.seed(123)
  rolls <- rhitpoints(iterations, skill, c(Count = 1, Faces = 6, Mod = -1))
  actual <- tabulate(rolls + 1L, nbins = 15L) / iterations
  expected <- dhitpoints(0:14, skill, c(Count = 1, Faces = 6, Mod = -1))
  expect_equal(actual, unname(expected), tolerance = 0.003)
})

test_that("random hit points match the AT12 1W6+2 distribution", {
  iterations <- iterations*3.5

  skill <- 12
  weapon <- c(Count = 1, Faces = 6, Mod = 2)

  set.seed(123)
  rolls <- rhitpoints(iterations, skill, weapon)
  actual <- tabulate(rolls + 1L, nbins = 17L) / iterations
  expected <- dhitpoints(0:16, skill, weapon)

  expect_equal(actual, unname(expected), tolerance = 0.003)
})

test_that("random hit points match the AT8 1W7+2 distribution", {
  iterations <- iterations*3.5

  skill <- 8
  weapon <- c(Count = 1, Faces = 7, Mod = 2)

  set.seed(123)
  rolls <- rhitpoints(iterations, skill, weapon)
  actual <- tabulate(rolls + 1L, nbins = 19L) / iterations
  expected <- dhitpoints(0:18, skill, weapon)

  expect_equal(actual, unname(expected), tolerance = 0.003)
})

test_that("random hit points match the AT11 2W6+0 distribution", {
  iterations <- iterations*5

  skill <- 11
  weapon <- c(Count = 2, Faces = 6, Mod = 0)

  set.seed(123)
  rolls <- rhitpoints(iterations, skill, weapon)
  actual <- tabulate(rolls + 1L, nbins = 25L) / iterations
  expected <- dhitpoints(0:24, skill, weapon)

  expect_equal(actual, unname(expected), tolerance = 0.003)
})

test_that("random hit points match the AT19 3W3+4 distribution", {
  iterations <- iterations*8

  skill <- 19
  weapon <- c(Count = 3, Faces = 3, Mod = 4)

  set.seed(123)
  rolls <- rhitpoints(iterations, skill, weapon)
  actual <- tabulate(rolls + 1L, nbins = 27L) / iterations
  expected <- dhitpoints(0:26, skill, weapon)

  expect_equal(actual, unname(expected), tolerance = 0.003)
})

test_that("random hit points include zero for a modifier below minimum", {
  iterations <- iterations*8

  skill <- 10
  weapon <- c(Count = 1, Faces = 6, Mod = -5)

  set.seed(123)
  rolls <- rhitpoints(iterations, skill, weapon)
  actual <- tabulate(rolls + 1L, nbins = 7L) / iterations
  expected <- dhitpoints(0:6, skill, weapon)

  expect_equal(actual, unname(expected), tolerance = 0.003)
})
