# 1=50.2375%, 2=27.2625%, 3=13.8250%, 4-6=0%
test_that("7, 14, 11 + 7 vector output matches http://dsa5.mueller-kalthoff.com/", {
  eav <- c(7, 14, 11)
  skill <- 7

  p <- seq(0, 1, 0.1)
  expect_equal(
    qql(p, eav, skill, format = "vector"),
    c(rep(1L, 5L), rep(2L, 3L), 3L, 4L, 4L) |>
      setNames(p)
  )
})


test_that("7, 14, 11 + 7 data frame output matches http://dsa5.mueller-kalthoff.com/", {
  eav <- c(7, 14, 11)
  skill <- 7
  p <- seq(0, 1, 0.1)

  expect_equal(
    qql(p, eav, skill, format = "df"),
    data.frame(
      QL = ordered(
        c(rep("Failed", 5L), rep("QL1", 3), "QL2", "QL3", "QL3"),
        levels = c("Failed", paste0("QL", 1:6))),
      p = p,
      row.names = p
    )
  )
})


#
# UPPER TAIL
#
# 1.0 0.502375 0.272625 0.13825 0.0 0.0 0.0
test_that("7, 14, 11 + 7 with upper tail matches http://dsa5.mueller-kalthoff.com/", {
  eav <- c(7, 14, 11)
  skill <- 7
  p <- seq(0, 1, 0.1)

  expect_equal(
    qql(p, eav, skill, lower.tail = FALSE, format="vector"),
    c(5, 4, 3, rep(2, 3), rep(1, 5)) |> setNames(p)
  )
})




#
# BOUNDARIES
#
# 0=1-0.00725, 1=0.00725
test_that("1, 1, 1 + 0  matches http://dsa5.mueller-kalthoff.com/", {
  eav <- c(1, 1, 1)
  skill <- 0
  p <- seq(0, 1, 0.1)

  expect_equal(
    qql(p, eav, skill),
    c(rep(1, 10), 2) |> setNames(p)
  )
})

# 1=10.3500%, 2=4.8500%, 3=2.4750%, 4=1.2250%, 5=0.7625%
test_that("1, 1, 1 + 15 matches http://dsa5.mueller-kalthoff.com/", {
  eav <- c(1, 1, 1)
  skill <- 15
  p <- seq(0, 1, 0.1)

  expect_equal(
    qql(p, eav, skill),
    c(rep(1, 9), 2, 6) |> setNames(p)
  )
})

# 1=99.2750%, 2=98.7750%, 3=97.5250%, 4=95.1500%, 5=91.3125%, 6=85.6750%
test_that("1, 1, 1 + 55 matches http://dsa5.mueller-kalthoff.com/", {
  eav <- c(1, 1, 1)
  skill <- 55
  p <- seq(0, 1, 0.1)

  expect_equal(
    qql(p, eav, skill),
    c(1, 6, rep(7, 9)) |> setNames(p)
  )
})

# 1-6 = 99.2750%
test_that("20, 20, 20 + 20 works matches http://dsa5.mueller-kalthoff.com/", {
  eav <- c(20, 20, 20)
  skill <- 20
  p <- seq(0, 1, 0.1)

  expect_equal(
    qql(p, eav, skill),
    c(1, rep(7, 10)) |> setNames(p)
  )
})


#
# EXCEPTIONS
#
test_that("p < 0 throws exception", {
  eav <- c(1, 10, 20)
  skill <- 11
  p <- seq(0, 1, 0.1)
  p[3L] <- -p[3L]

  expect_error(
    qql(p, eav, skill)
  )
})


# eav less than 1
test_that("eav[i] <= 0 throws exception", {
  eav <- sample(c(1, 0, 20)) # regardless of the order
  skill <- 11
  p <- seq(0, 1, 0.1)

  expect_error(
    qql(p, eav, skill)
  )
})

# skill less than 0
test_that("skill < 0 throws exception", {
  eav <- sample(c(1, -10, 20)) # regardless of the order
  skill <- -1
  p <- seq(0, 1, 0.1)

  expect_error(
    qql(p, eav, skill)
  )
})
