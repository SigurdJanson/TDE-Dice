test_that("strings in correct format work", {
  expect_equal(d("d6"), newDice(1, 6))
  expect_equal(d("1d17"), newDice(1, 17))
  expect_equal(d("2d6"), newDice(2, 6))
  expect_equal(d("2d3"), newDice(2, 3))
  expect_equal(d("3d20"), newDice(3, 20))
})


test_that("unnamed vectors in correct format work", {
  expect_equal(d(6), newDice(1, 6))
  expect_equal(d(c(1, 17)), newDice(1, 17))
  expect_equal(d(c(2,  6)), newDice(2,  6))
  expect_equal(d(c(2,  3)), newDice(2,  3))
  expect_equal(d(c(3, 20)), newDice(3, 20))
})


test_that("Named vectors with full names in correct format work", {
  expect_equal(d(c(Faces = 6)), newDice(1, 6))
  expect_equal(d(c(Count = 2,  Faces = 6)), newDice(2,  6))
})

test_that("Named vectors with partial names in correct format work", {
  expect_equal(d(c(F = 100)), newDice(1, 100))
  expect_equal(d(c(C = 2,  Fa = 18)), newDice(2,  18))
})


#
# CONTENT EXCEPTIONS
#
test_that("too few dice throw an error", {
  expect_error(d("0d6"))
})
test_that("strings without die faces throw an error", {
  expect_error(d("2d"))
})
test_that("too few faces throw an error", {
  expect_error(d("1d1"))
})


#
# ARGUMENT EXCEPTIONS
#
test_that("more than 1 string throws an error", {
  expect_error(d(c("2d6", "1d6")))
})

test_that("less than 1 number throws an error", {
  expect_error(d(integer()))
})
test_that("more than 2 numbers throw an error", {
  expect_error(d(c(2, 3, 4)))
})

test_that("types other than numeric or string throw an error", {
  expect_error(d(list), "Dice format not recognized", fixed = TRUE)
  expect_error(d(complex(1)), "Dice format not recognized", fixed = TRUE)
})
