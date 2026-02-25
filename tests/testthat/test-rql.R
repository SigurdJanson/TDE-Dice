
test_that("function returns consistent results with consistent seed", {
  eav <- sample(1:20, 3)
  skill <- sample(0:20, 1)
  #-cat("DEBUG:", )

  set.seed(123)
  expected <- rql(13, eav, skill)

  set.seed(123)
  expect_equal(rql(13, eav, skill), expected)
})


test_that("outcomes are between 0 and 3", {
  eav <- sample(1:20, 3)
  skill <- sample(0:9, 1) # max skill points of 6 give 3 QL
  results <- replicate(200, rql(13, eav, skill))

  expect_true(all(results >= 0))
  expect_true(all(results <= 3))
})


test_that("outcomes are between 0 and 2", {
  eav <- sample(1:20, 3)
  skill <- sample(0:6, 1) # max skill points of 6 give 2 QL
  results <- replicate(200, rql(13, eav, skill))

  expect_true(all(results >= 0))
  expect_true(all(results <= 2))
})


test_that("rql() has the correct weighted distribution", {
  n <- 7500
  eav <- c(5, 11, 17)
  skill <- 5

  rolls <- replicate(n, rql(1, eav, skill))

  # The expected proportion
  expectedQL2 <- 0.16075
  expectedQL1 <- 0.34975 - expectedQL2
  expectedFailure <- 1 - 0.34975
  expected <- c(expectedFailure, expectedQL1, expectedQL2, rep(0.0, 4L))

  # Allow for a 2% margin of error due to randomness
  for (ql in 0:6) {
    actualQL <- sum(rolls == ql) / n
    tol <- ifelse(ql > 2, 0.0, 0.02)
    expect_equal(actualQL, expected[ql+1L], tolerance = tol, info = paste0("ql", ql))
  }
})


test_that("n = 0 returns an empty set", {
  eav <- sample(1:20, 3)
  skill <- sample(0:20, 1)

  expect_equal(rql(0, eav, skill), integer())
})
test_that("length(n) > 1 , then length(n) is used for n", {
  eav <- sample(1:20, 3)
  skill <- sample(0:20, 1)
  len <- sample(2:10, 1)

  expect_length(rql(rep(0, len), eav, skill), len)
})


#
# EXCEPTIONS
#
test_that("length(eav) > 3 throws exception", {
  eav <- sample(1:20, 4)
  skill <- sample(0:20, 1)
  expect_error(rql(1L, eav, skill))
})
test_that("length(eav) < 3 throws exception", {
  eav <- sample(1:20, 4)
  skill <- sample(0:20, 1)
  expect_error(rql(1L, eav, skill))
})

test_that("eav = 0 throws exception", {
  eav <- sample(1:20, 3)
  eav[sample(1:3, 1)] <- 0L
  skill <- sample(0:20, 1)
  expect_error(rql(1L, eav, skill))
})

test_that("eav = NULL throws exception", {
  skill <- sample(0:20, 1)
  expect_error(rql(1L, NULL, skill))
})
test_that("eav = NA throws exception", {
  skill <- sample(0:20, 1)
  expect_error(rql(1L, NA, skill))
})

