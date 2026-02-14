test_that("eav = 1 returns values increasing by 1/20", {
  expect_equal(
    pRect1d20(1:20, 1L),
    setNames(seq(1/20, 1, 1/20), 1:20)
  )
})
test_that("eav = 11 returns correct result", {
  expect_equal(
    pRect1d20(1:20, 11L),
    setNames(c(rep(0, 10), 11/20, seq(12/20, 1, 1/20)), 1:20)
  )
})
test_that("eav = 1 returns values increasing by 1/20", {
  select <- c(3, 7, 10, 14)
  expect_equal(
    pRect1d20(select, 1L),
    setNames(c(3/20, 7/20, 1/2, 14/20), select)
  )
})


#
# ARGUMENT lower.trail
#
test_that("eav = 1 and lower.tail=F returns correct result", {
  expect_equal(
    pRect1d20(1:20, 1L, lower.tail=FALSE),
    setNames(seq(1, 1/20, -1/20), 1:20)
  )
})
test_that("eav = 1 and lower.tail=F returns correct result", {
  select <- c(3, 7, 10, 14)
  expect_equal(
    pRect1d20(select, 1L, lower.tail=FALSE),
    setNames(c(18/20, 14/20, 11/20, 7/20), select)
  )
})


#
# EXCEPTIONS
#
test_that("length(eav) > 1 throws exception", {
  expect_error(pRect1d20(1:20, 1:2L))
})
test_that("length(eav) < 1 throws exception", {
  expect_error(pRect1d20(1:20, numeric()))
})

test_that("eav = 0 throws exception", {
  expect_error(pRect1d20(1:20, 0))
})
test_that("eav = 21 throws exception", {
  expect_error(pRect1d20(1:20, 21))
})

test_that("eav = NULL throws exception", {
  expect_error(pRect1d20(1:20, NULL))
})
test_that("eav = NA throws exception", {
  expect_error(pRect1d20(5, NA))
})


test_that("q = NULL throws exception", {
  expect_error(pRect1d20(NULL, 2))
})
test_that("q = NA throws exception", {
  expect_error(pRect1d20(NA, 2))
})
