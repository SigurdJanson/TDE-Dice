test_that("3d20", {
  expected <- c(0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75,
                2, 2.25, 2.5, 2.75, 3, 3.25, 3.5, 3.75,
                4, 4.25, 4.5, 4.75, 5, 4.75, 4.5, 4.25,
                4, 3.75, 3.5, 3.25, 3, 2.75, 2.5, 2.25,
                2, 1.75, 1.5, 1.25, 1, 0.75, 0.5, 0.25) / 100
  expect_equal(
    dSum2dN(20L),
    setNames(expected, 2:40)
  )
})

test_that("2d6", {
  expected <- c(1, 2, 3, 4, 5,
                6, 5, 4, 3, 2, 1) / 36
  expect_equal(
    dSum2dN(6),
    setNames(expected, 2:12)
  )
})

test_that("2d3", {
  expected <- c(1, 2, 3, 2, 1) / 9
  expect_equal(
    dSum2dN(3),
    setNames(expected, 2:6)
  )
})

test_that("2d13", {
  expected <- c(0.591715976331, 1.18343195266, 1.77514792899,
                2.36686390533, 2.95857988166, 3.55029585799,
                4.14201183432, 4.73372781065, 5.32544378698,
                5.91715976331, 6.50887573964, 7.10059171598,
                7.69230769231, 7.10059171598, 6.50887573964,
                5.91715976331, 5.32544378698, 4.73372781065,
                4.14201183432, 3.55029585799, 2.95857988166,
                2.36686390533, 1.77514792899, 1.18343195266,
                0.591715976331 ) / 100
  expect_equal(
    dSum2dN(13),
    setNames(expected, 2:26)
  )
})




#
# ARGUMENT EXCEPTIONS
#
test_that("argument < 1 causes an exception", {
  expect_error(
    dSum2dN(0)
  )
})

test_that("argument NULL causes an exception", {
  expect_error(
    dSum2dN(NULL)
  )
})

test_that("argument NA causes an exception", {
  expect_error(
    dSum2dN(NA)
  )
})
