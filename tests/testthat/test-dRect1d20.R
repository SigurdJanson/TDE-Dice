test_that("eav = 1 returns vector with 20 times the value 1/20", {
  expect_equal(
    dRect1d20(1:20, 1L),
    setNames(rep(1/20, 20), 1:20)
  )
})

test_that("eav = 1 and x returns partial vector with length(x) times the value 1/20", {
  select <- 5:8
  expect_equal(
    dRect1d20(select, 1L),
    setNames(rep(1/20, length(select)), select)
  )
})

test_that("eav = 5 returns correct result", {
  expect_equal(
    dRect1d20(1:20, 5L),
    setNames(c(rep(0, 4), 5/20, rep(1/20, 15)), 1:20)
  )
})
test_that("eav = 5 and x = 4 returns 0", {
  expect_equal(
    dRect1d20(4L, 5L),
    setNames(0, 4)
  )
})
test_that("eav = 5 and x = 5 returns 1/4", {
  expect_equal(
    dRect1d20(5L, 5L),
    setNames(5/20, 5)
  )
})
test_that("eav = 5 and x = 6 returns 1/20", {
  expect_equal(
    dRect1d20(6L, 5L),
    setNames(1/20, 6)
  )
})

test_that("eav = 20 returns correct result", {
  expect_equal(
    dRect1d20(1:20, 20L),
    setNames(c(rep(0, 19), 1), 1:20L)
  )
})


#
# EXCEPTIONS
#
test_that("length(eav) > 1 throws exception", {
  expect_error(dRect1d20(1:20, 1:2L))
})
test_that("length(eav) < 1 throws exception", {
  expect_error(dRect1d20(1:20, numeric()))
})

test_that("eav = 0 throws exception", {
  expect_error(dRect1d20(1:20, 0))
})
test_that("eav = 21 throws exception", {
  expect_error(dRect1d20(1:20, 21))
})

test_that("eav = NULL throws exception", {
  expect_error(dRect1d20(1:20, NULL))
})
test_that("eav = NA throws exception", {
  expect_error(dRect1d20(5, NA))
})


test_that("x = NULL throws exception", {
  expect_error(dRect1d20(NULL, 2))
})
test_that("x = NA throws exception", {
  expect_error(dRect1d20(NA, 2))
})
