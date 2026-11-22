test_that("rCompete generates values from the difference support", {
  dFixed <- \(x) ifelse(x == 1, 1, 0)

  expect_equal(rCompete(10, dFixed, 0:5), rep(0, 10))
  expect_equal(rCompete(10, dFixed, 0:5, ties = FALSE), rep(-1, 10))
})



#
# BOUNDARIES ####
#
test_that("rCompete respects n conventions", {
  dCoins <- \(x) ifelse(x == 1, 0.5, ifelse(x == 2, 0.5, 0))

  expect_length(rCompete(0, dCoins, 0:3), 0)
  expect_length(rCompete(c(1, 2, 3), dCoins, 0:3), 3)
})

test_that("n = 0 returns an empty set", {
  dFixed <- \(x) ifelse(x == 1, 1, 0)

  expect_length(rCompete(numeric(), dFixed, 0:3), 0)
})



#
# EXCEPTIONS
#
test_that("rCompete throws error when n is invalid", {
  dCoins <- \(x) ifelse(x == 1, 0.5, ifelse(x == 2, 0.5, 0))

  expect_error(rCompete(-1, dCoins, 0:3), "'n' must")
  expect_error(rCompete(1.5, dCoins, 0:3), "'n' must")
})

test_that("rCompete returns NA for invalid distributions", {
  dInvalid <- \(x) ifelse(x == 1, -1, 0)

  expect_equal(rCompete(3, dInvalid, 0:3), rep(NA_real_, 3))
})
