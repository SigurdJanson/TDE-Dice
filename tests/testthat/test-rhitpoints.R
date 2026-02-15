
test_that("function returns consistent results with consistent seed", {
  eav <- sample(1:20, 1)
  diceCount <- sample(1:5, 1)
  weapon <- c(Count = diceCount, Dice = sample(2:20, 1), Mod = sample(-diceCount:5, 1))
  #-cat("DEBUG:", weapon)

  set.seed(123)
  expected <- rhitpoints(12, eav, weapon)

  set.seed(123)
  expect_equal(rhitpoints(12, eav, weapon), expected)
})


test_that("outcomes are between 1 and 12", {
  eav <- sample(1:20, 1)
  weapon <- c(Count = 1, Dice = 6, Mod = 0)
  results <- replicate(200, rhitpoints(1, eav, weapon))

  expect_true(all(results >= 0))
  expect_true(all(results <= 12))
})


test_that("outcomes are between 3 and 26", {
  eav <- sample(20, 1)
  weapon <- c(Count = 2, Dice = 6, Mod = 1)

  results <- replicate(200, rhitpoints(1, eav, weapon))

  expect_true(all(results >= 3 | results == 0))
  expect_true(all(results <= 26))
})


test_that("d20 has the correct weighted distribution", {
  eav <- 5
  n <- 5000
  weapon <- c(Count = sample(1:5, 1), Dice = sample(2:20, 1), Mod = sample(-5:5, 1))
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


test_that("n = 0 returns an empty set", {
  weapon <- c(Count = sample(1:5, 1), Dice = sample(2:20, 1), Mod = sample(-5:5, 1))
  expect_equal(rhitpoints(0, 12, weapon), integer())
})
test_that("length(n) > 1 , then length(n) is used for n", {
  weapon <- c(Count = sample(1:5, 1), Dice = sample(2:20, 1), Mod = sample(-5:5, 1))
  len <- sample(2:10, 1)

  expect_length(rhitpoints(rep(0, len), 11, weapon), len)
})


#
# EXCEPTIONS
#
test_that("length(eav) > 1 throws exception", {
  eav <- 10:11
  weapon <- c(Count = sample(1:5, 1), Dice = sample(2:20, 1), Mod = sample(-5:5, 1))
  expect_error(rhitpoints(1, eav, weapon))
})
test_that("length(eav) < 1 throws exception", {
  eav <- numeric()
  weapon <- c(Count = sample(1:5, 1), Dice = sample(2:20, 1), Mod = sample(-5:5, 1))
  expect_error(rhitpoints(1:20, eav, weapon))
})

test_that("eav = 0 throws exception", {
  eav <- 0
  weapon <- c(Count = sample(1:5, 1), Dice = sample(2:20, 1), Mod = sample(-5:5, 1))
  expect_error(rhitpoints(1:20, 0))
})
test_that("eav = 21 throws exception", {
  eav <- 21
  weapon <- c(Count = sample(1:5, 1), Dice = sample(2:20, 1), Mod = sample(-5:5, 1))
  expect_error(rhitpoints(1:20, 21, weapon))
})

test_that("eav = NULL throws exception", {
  expect_error(rhitpoints(1:20, NULL, weapon))
})
test_that("eav = NA throws exception", {
  expect_error(rhitpoints(5, NA, weapon))
})

