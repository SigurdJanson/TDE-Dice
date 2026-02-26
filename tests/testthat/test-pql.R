# 1=50.2375%, 2=27.2625%, 3=13.8250%, 4-6=0%
test_that("7, 14, 11 + 7 vector output matches http://dsa5.mueller-kalthoff.com/", {
  eav <- c(7, 14, 11)
  skill <- 7
  expect_equal(
    pql(0:6, eav, skill, format="vector"),
    c(1-0.502375, 0.502375-0.272625, 0.272625-0.13825, 0.13825, rep(0, 3L)) |>
      cumsum() |>
      setNames(c("Failed", paste0("QL", 1:6)))
  )
})

test_that("7, 14, 11 + 7 data frame output matches http://dsa5.mueller-kalthoff.com/", {
  eav <- c(7, 14, 11)
  skill <- 7
  expect_equal(
    pql(0:6, eav, skill, format="df"),
    data.frame(
      QL = ordered(c("Failed", paste0("QL", 1:6))),
      p = c(1-0.502375, 0.502375-0.272625, 0.272625-0.13825, 0.13825, rep(0, 3)) |>
        cumsum()
    )
  )
})

#
# UPPER TAIL
#
test_that("7, 14, 11 + 7 vector output matches http://dsa5.mueller-kalthoff.com/", {
  eav <- c(7, 14, 11)
  skill <- 7
  expect_equal(
    pql(0:6, eav, skill, lower.tail = FALSE, format="vector"),
    c(1, 0.502375, 0.272625, 0.13825, rep(0, 3L)) |>
      setNames(c("Failed", paste0("QL", 1:6)))
  )
})

test_that("7, 14, 11 + 7 data frame output matches http://dsa5.mueller-kalthoff.com/", {
  eav <- c(7, 14, 11)
  skill <- 7
  expect_equal(
    pql(0:6, eav, skill, lower.tail = FALSE, format="df"),
    data.frame(
      QL = ordered(c("Failed", paste0("QL", 1:6))),
      p = c(1, 0.502375, 0.272625, 0.13825, rep(0, 3))
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
    pql(0:6, eav, skill),
    setNames(c(1-0.00725, 1, rep(1, 5)),
             c("Failed", paste0("QL", 1:6)))
  )
})

# 1-6 = 99.2750%
test_that("20, 20, 20 + 20 works matches http://dsa5.mueller-kalthoff.com/", {
  eav <- c(20, 20, 20)
  skill <- 20
  expect_equal(
    pql(0:6, eav, skill),
    setNames(c(rep(1-0.992750, 6), 1),
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
    pql(2:4, eav, skill),
    setNames(rep(1, 3L),
             paste0("QL", 2:4))
  )
})
test_that("10, 10, 10 + 4 with upper tail works", {
  eav <- c(10, 10, 10)
  skill <- 4
  expect_equal(
    pql(2:4, eav, skill, lower.tail = FALSE),
    setNames(c(0.12875, 0, 0),
             paste0("QL", 2:4))
  )
})

test_that("10, 10, 10 + 4 works for data frame format", {
  eav <- c(10, 10, 10)
  skill <- 4
  expect_equal(
    pql(2:4, eav, skill, format = "df"),
    data.frame(
      QL = paste0("QL", 2:4) |> ordered(levels = c("Failed", paste0("QL", 1:6))),
      p = rep(1, 3L),
      row.names = 3:5)
  )
})
test_that("10, 10, 10 + 4 with upper tail works for data frame format", {
  eav <- c(10, 10, 10)
  skill <- 4
  expect_equal(
    pql(2:4, eav, skill, lower.tail = FALSE, format = "df"),
    data.frame(
      QL = paste0("QL", 2:4) |> ordered(levels = c("Failed", paste0("QL", 1:6))),
      p = c(0.12875, 0, 0),
      row.names = 3:5)
  )
})

test_that("10, 10, 10 + 4, x=0", {
  eav <- c(10, 10, 10)
  skill <- 4
  expect_equal(
    pql(0, eav, skill),
    setNames(1-0.30025, "Failed")
  )
})
test_that("10, 10, 10 + 4, x=6 returns 0", {
  eav <- c(10, 10, 10)
  skill <- 4
  expect_equal(
    pql(6, eav, skill),
    setNames(1.0, "QL6")
  )
})



#
# EXCEPTIONS
#
test_that("q < 0 throws exception", {
  eav <- c(1, 10, 20)
  skill <- 11
  expect_error(
    pql(-1, eav, skill)
  )
})
test_that("q > 6 throws exception", {
  eav <- c(5, 10, 15)
  skill <- 11
  expect_error(
    pql(7, eav, skill)
  )
})

# eav less than 1
test_that("eav[i] < 0 throws exception", {
  eav <- sample(c(1, -10, 20)) # regardless of the order
  skill <- 11
  expect_error(
    pql(1, eav, skill)
  )
})
