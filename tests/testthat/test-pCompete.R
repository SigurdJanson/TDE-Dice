test_that("pCompete calculates lower-tail probabilities", {
  dCoins <- \(x) ifelse(x == 1, 0.5, ifelse(x == 2, 0.5, 0))

  expect_equal(
    pCompete(c(-2, -1, 0, 1), dCoins, 0:3),
    c(0, 0.25, 0.75, 1)
  )
})


test_that("pCompete calculates upper-tail probabilities", {
  dCoins <- \(x) ifelse(x == 1, 0.5, ifelse(x == 2, 0.5, 0))

  expect_equal(
    pCompete(c(-2, -1, 0, 1), dCoins, 0:3, lower.tail = FALSE),
    c(1, 0.75, 0.25, 0)
  )
})


test_that("pCompete supports ties = FALSE", {
  dFixed <- \(x) ifelse(x == 1, 1, 0)

  expect_equal(pCompete(c(-2, -1, 0), dFixed, 0:5, ties = FALSE),
               c(0, 1, 1))
})


test_that("pCompete handles invalid distributions and validates inputs", {
  dInvalid <- \(x) ifelse(x == 1, -1, 0)

  expect_equal(pCompete(c(-1, 0), dInvalid, 0:3),
               c(NA_real_, NA_real_))
  expect_error(pCompete(c(NA_real_, 0), \(x) rep(0.5, length(x)), 0:3))
})



#
# BOUNDARIES ####
#

# So far, none




#
# EXCEPTIONS ####
#
test_that("Invalid `lower.tail` throws exception", {
  dCoins <- \(x) ifelse(x == 1, 0.5, ifelse(x == 2, 0.5, 0))

  expect_error(pCompete(q=-3:3, dfun = dCoins, x  = 0:3, ties = FALSE, lower.tail=NA))
  expect_error(pCompete(q=-3:3, dfun = dCoins, x  = 0:3, ties = FALSE, lower.tail=c(TRUE, FALSE)))
  expect_error(pCompete(q=-3:3, dfun = dCoins, x  = 0:3, ties = FALSE, lower.tail=28L))
  expect_no_error(pCompete(q=-3:3, dfun = dCoins, x  = 0:3, ties = FALSE, lower.tail=TRUE))
})
