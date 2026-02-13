









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


test_that("p = NULL throws exception", {
  expect_error(pRect1d20(NULL, 2))
})
test_that("p = NA throws exception", {
  expect_error(pRect1d20(NA, 2))
})
