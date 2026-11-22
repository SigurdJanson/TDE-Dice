

test_that("Degenerate/equality case yields 100% ties", {
  dFixed <- \(x) ifelse(x==1, 1, 0)

  result <- dCompete(dfun = dFixed, x = 0:5)
  expect_equal(result,
               c(0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0) |>
                 setNames(-5:5))

  result <- dCompete(dfun = dFixed, x  = 0:5, ties = FALSE)
  expect_equal(result,
               c(0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0) |>
                 setNames(-5:5))
})


test_that("Two fair coins", {
  dCoins <- \(x) ifelse(x==1, 0.5, ifelse(x==2, 0.5, 0))

  result <- dCompete(dfun = dCoins, x = 0:3)
  expect_equal(result,
               c(0, 0, 0.25, 0.5, 0.25, 0, 0) |>
                 setNames(-3:3))

  result <- dCompete(dfun = dCoins, x  = 0:3, ties = FALSE)
  expect_equal(result,
               c(0, 0, 0.75, 0, 0.25, 0, 0) |>
                 setNames(-3:3))
})

test_that("Two loaded coins", {
  dCoins <- \(x) ifelse(x==1, 0.3, ifelse(x==2, 0.7, 0))

  result <- dCompete(dfun = dCoins, x = 0:3)
  expect_equal(result,
               c(0, 0, 0.21, 0.58, 0.21, 0, 0) |>
                 setNames(-3:3))

  result <- dCompete(dfun = dCoins, x  = 0:3, ties = FALSE)
  expect_equal(result,
               c(0, 0, 0.21 + 0.58, 0.0, 0.21, 0, 0) |>
                 setNames(-3:3))
})

#
# test_that("Identical distributions yield 50:50", {
#   prob <- 0.5
#
#   for (i in 1:5) {
#     size <- sample(5:25, 1L)
#     result <- dCompete(
#       dfun = dbinom,
#       x    = 0:10,
#       aArgs = list(size = size, prob = prob),
#       pArgs = list(size = size, prob = prob)
#     )
#
#     expect_equal(result[["aWins"]], result[["pWins"]])
#   }
# })

#
# BOUNDARIES
#

# So far, none


#
# EXCEPTIONS
#

# So far, none
