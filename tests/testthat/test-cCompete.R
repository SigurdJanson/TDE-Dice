

test_that("Poisson(5) vs Poisson(3) returns correct result",{
  expected <- c(aWins = 0.70180660362599633917,
                Tie   = 0.11313216886055542376,
                pWins = 0.18506122751344383781)
  result <- cCompete(dpois, 0:30, list(lambda = 5), list(lambda = 3))

  expect_equal(result, expected)
})


test_that("Degenerate/equality case yields 100% ties", {
  dFixed <- \(x) ifelse(x==1, 1, 0)

  result <- cCompete(dfun = dFixed, x = 0:5)
  expect_equal(c(aWins = 0.0, Tie = 1.0, pWins = 0.0), result)

  result <- cCompete(dfun = dFixed, x  = 0:10, ties = FALSE)
  expect_equal(result, c(aWins = 0.0, Tie = 0.0, pWins = 1.0))
})


test_that("Two fair coins", {
  dCoins <- \(x) ifelse(x==1, 0.5, ifelse(x==2, 0.5, 0))

  result <- cCompete(dfun = dCoins, x = 0:3)
  expect_equal(c(aWins = 0.250, Tie = 0.5, pWins = 0.250), result)

  result <- cCompete(dfun = dCoins, x  = 0:10, ties = FALSE)
  expect_equal(result, c(aWins = 0.25, Tie = 0.0, pWins = 0.75))
})

test_that("Two loaded coins", {
  dCoins <- \(x) ifelse(x==1, 0.3, ifelse(x==2, 0.7, 0))

  result <- cCompete(dfun = dCoins, x = 0:3)
  expect_equal(c(aWins = 0.21, Tie = 0.58, pWins = 0.21), result)

  result <- cCompete(dfun = dCoins, x  = 0:10, ties = FALSE)
  expect_equal(result, c(aWins =  0.21, Tie = 0.0, pWins = 0.21 + 0.58))
})


test_that("Identical distributions yield 50:50", {
  prob <- 0.5

  for (i in 1:5) {
    size <- sample(5:25, 1L)
    result <- cCompete(
      dfun = dbinom,
      x    = 0:10,
      aArgs = list(size = size, prob = prob),
      pArgs = list(size = size, prob = prob)
    )

    expect_equal(result[["aWins"]], result[["pWins"]])
  }
})

#
# BOUNDARIES
#

# So far, none


#
# EXCEPTIONS
#

# So far, none
