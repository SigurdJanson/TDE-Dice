test_that("newDice returns object", {
  expect_s3_class(newDice(2, 6), "Dice")
})

test_that("newDice returns object", {
  expect_equal(newDice(2, 6), structure(
    c(Count = 2, Faces = 6),
    class = c("Dice")
  ))
})

#
# IS
#
test_that("`is.Dice()` confirms that newRoll returns a Dice object", {
  expect_true(is.Dice(newDice(2, 6)))
})
test_that("`is.Dice()` confirms that newRoll returns a Dice object", {
  expect_false(is.Dice(c(2, 6)))
})


#
# PRINT
#
test_that("print a Dice object", {
  #print(newDice(2, 6))
  expect_output(print(newDice(2, 6)), "2d6")
})
test_that("print a Dice object", {
  expect_output(print(newDice(3, 20)), "3d20")
})


#
# EXCEPTIONS
#
test_that("Zero dice throw exception", {
  expect_error(newDice(0, 6))
})
test_that("1 face throws exception", {
  expect_error(newDice(2, 1))
})

test_that("Wrong count type throws exception", {
  expect_error(newDice("2", 6))
})
test_that("Wrong face type throws exception", {
  expect_error(newDice(2, "6"))
})

test_that("Fractional dice throw exception", {
  expect_error(newDice(2.2, 6))
})
test_that("Fractional face throws exception", {
  expect_error(newDice(2, 5.9))
})
