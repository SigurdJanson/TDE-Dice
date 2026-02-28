test_that("exact matches return according index", {
  targets <- c(0.0, 0.3, 0.5, 0.9, 1.0)
  x <- seq(0.0, 1.0, 0.1)

  for (i in seq_along(targets)) {
    expect_equal(
      findFirstGE(x, targets[i]),
      which(abs(x - targets[i]) < .Machine$double.eps^0.5),
      info = paste0("target=", targets[i])
    )
  }
})


test_that("Slightly higher targets return index + 1", {
  # do not test 1.0 because that would exceed the range of `x`
  targets <- c(0.0, 0.3, 0.5, 0.9)
  x <- seq(0.0, 1.0, 0.1)

  for (i in seq_along(targets)) {
    expect_equal(
      findFirstGE(x, targets[i] + 0.0001),
      which(abs(x - targets[i]) < .Machine$double.eps^0.5) + 1L,
      info = paste0("target=", targets[i])
    )
  }
})


test_that("Slightly lower targets return same index", {
  targets <- c(0.0, 0.3, 0.5, 0.9, 1.0)
  x <- seq(0.0, 1.0, 0.1)

  for (i in seq_along(targets)) {
    expect_equal(
      findFirstGE(x, targets[i] - 0.0001),
      which(abs(x - targets[i]) < .Machine$double.eps^0.5),
      info = paste0("t=", t)
    )
  }
})




#
# BOUNDARIES
#
test_that("Lower than `min(x)` returns `1`", {
  Precision <- 1E-10
  min <- -1
  max <- 12.0
  x <- seq(0.0, 1.0, (max-min) / 12)

  expect_equal(findFirstGE(x, min - Precision), 1)
})

test_that("Greater than `max(x)` returns `NA`", {
  Precision <- 1E-10
  min <- 0.0
  max <- 13.0
  x <- seq(0.0, 1.0, (max-min) / 12)

  expect_equal(findFirstGE(x, max + Precision), NA_integer_)
})


#
# EDGE CASES
#
test_that("Empty `x` returns empty result", {
  target <- 0.5
  x <- numeric()

  expect_equal(findFirstGE(x, target), numeric())
})

test_that("Empty `target` returns empty result", {
  target <- numeric()
  x <- seq(0.0, 1.0, 0.2)

  expect_equal(findFirstGE(x, target), numeric())
})

test_that("`target = NA` returns NA", {
  target <- NA
  x <- seq(0.0, 1.0, 0.13)
  expect_equal(findFirstGE(x, target), NA_real_)
})
test_that("`target[i] = NA` returns ...[i] == NA", {
  target <- c(0, 0.2, NA, 0.5)
  x <- seq(0.0, 1.0, 0.13)
  expect_equal(findFirstGE(x, target), c(1L, 3L, NA, 5L))
})

test_that("any `x[i] == NA` is ignored as long as it contains valid values", {
  target <- 0.5
  x <- seq(0.0, 1.0, 0.13)
  x[5L] <- NA

  expect_equal(findFirstGE(x, target), 6L)
})




#
# EXCEPTIONS
#
test_that("`all.na(x)` returns empty result", {
  target <- c(0.5, 0.7)
  x <- rep(NA_real_, 5)

  expect_equal(findFirstGE(x, target), rep(NA_integer_, 2))
})

test_that("`x` is not a vector", {
  target <- 0.6
  x <- seq(0.0, 1.0, 0.1) |> as.list()

  expect_error(findFirstGE(x, target))
})
test_that("`x` is not numeric", {
  target <- 0.6
  x <- seq(0.0, 1.0, 0.1) |> as.character()

  expect_error(findFirstGE(x, target))
})

test_that("`target` is not numeric", {
  target <- "0.6"
  x <- seq(0.0, 1.0, 0.1)

  expect_error(findFirstGE(x, target))
})
