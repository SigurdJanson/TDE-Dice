# 1=50.2375%, 2=27.2625%, 3=13.8250%, 4-6=0%
test_that("7, 14, 11 + 7 vector output matches http://dsa5.mueller-kalthoff.com/", {
  eav <- c(7, 14, 11)
  skill <- 7
  expect_equal(
    dql(0:6, eav, skill, "vector"),
    setNames(c(1-0.502375, 0.502375-0.272625, 0.272625-0.13825, 0.13825, rep(0, 3)),
             c("Failed", paste0("QL", 1:6)))
  )
})

test_that("7, 14, 11 + 7 data frame output matches http://dsa5.mueller-kalthoff.com/", {
  eav <- c(7, 14, 11)
  skill <- 7
  expect_equal(
    dql(0:6, eav, skill, "df"),
    data.frame(
      QL = ordered(c("Failed", paste0("QL", 1:6))),
      p = c(1-0.502375, 0.502375-0.272625, 0.272625-0.13825, 0.13825, rep(0, 3))
    )
  )
})


#
# BOUNDARIES
#
test_that("1, 1, 1 + 0  matches http://dsa5.mueller-kalthoff.com/", {
  eav <- c(1, 1, 1)
  skill <- 0
  expect_equal(
    dql(0:6, eav, skill),
    setNames(c(1-0.00725, 0.00725, rep(0, 5)),
             c("Failed", paste0("QL", 1:6)))
  )
})

# 1=10.3500%, 2=4.8500%, 3=2.4750%, 4=1.2250%, 5=0.7625%
test_that("1, 1, 1 + 15 matches http://dsa5.mueller-kalthoff.com/", {
  eav <- c(1, 1, 1)
  skill <- 15
  expect_equal(
    dql(0:6, eav, skill),
    setNames(
      c(1-0.1035, 0.055, 0.02375, 0.0125, 0.004625, 0.007625, 0),
      c("Failed", paste0("QL", 1:6))
    )
  )
})


# 1=99.2750%, 2=98.7750%, 3=97.5250%, 4=95.1500%, 5=91.3125%, 6=85.6750%
test_that("1, 1, 1 + 55 matches http://dsa5.mueller-kalthoff.com/", {
  eav <- c(1, 1, 1)
  skill <- 55
  expect_equal(
    dql(0:6, eav, skill),
    setNames(
      c(1-0.99275, 0.005, 0.0125, 0.02375, 0.038375, 0.056375, 0.85675),
      c("Failed", paste0("QL", 1:6))
    )
  )
})


test_that("1, 1, 1 + 0 works matches http://dsa5.mueller-kalthoff.com/", {
  eav <- c(1, 1, 1)
  skill <- 0
  expect_equal(
    dql(0:6, eav, skill),
    setNames(c(1-0.007250, 0.007250, rep(0, 5)),
             c("Failed", paste0("QL", 1:6)))
  )
})

# 1-6 = 99.2750%
test_that("20, 20, 20 + 20 works matches http://dsa5.mueller-kalthoff.com/", {
  eav <- c(20, 20, 20)
  skill <- 20
  expect_equal(
    dql(0:6, eav, skill),
    setNames(c(1-0.992750, rep(0, 5L), 0.992750),
             c("Failed", paste0("QL", 1:6)))
  )
})


#
#  SUBSETTING
#
# 1=30.0250%, 2=12.8750%, >3= 0
test_that("10, 10, 10 + 4 works", {
  eav <- c(10, 10, 10)
  skill <- 4
  expect_equal(
    dql(2:4, eav, skill),
    setNames(c(0.128750, rep(0, 2)),
             c(paste0("QL", 2:4)))
  )
})

test_that("10, 10, 10 + 4, x=0", {
  eav <- c(10, 10, 10)
  skill <- 4
  expect_equal(
    dql(0, eav, skill),
    setNames(1-0.30025, "Failed")
  )
})
test_that("10, 10, 10 + 4, x=6 returns 0", {
  eav <- c(10, 10, 10)
  skill <- 4
  expect_equal(
    dql(6, eav, skill),
    setNames(0.0, "QL6")
  )
})

#
# EXCEPTIONS
#
test_that("x < 0 throws exception", {
  eav <- c(1, 10, 20)
  skill <- 11
  expect_error(
    dql(-1, eav, skill)
  )
})


# eav less than 1
test_that("eav[i] <= 0 throws exception", {
  eav <- sample(c(1, 0, 20)) # regardless of the order
  skill <- 11
  expect_error(
    dql(1, eav, skill)
  )
})
