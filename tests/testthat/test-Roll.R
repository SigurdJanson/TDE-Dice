test_that("`newRoll` returns Roll object", {
  expect_s3_class(newRoll(2, 6), "Roll")
})

test_that("newRoll returns object", {
  expect_equal(newRoll(2, 6, 2), structure(
    c(Count = 2, Faces = 6, Mod = 2),
    class = c("Roll")
  ))
})


#
# IS
#
test_that("`is.Roll()` confirms that newRoll returns Roll object", {
  expect_true(is.Roll(newRoll(2, 6)))
})
test_that("`is.Roll()` confirms that newRoll returns Roll object", {
  expect_false(is.Roll(c(2, 6)))
})


#
# PRINT
#
test_that("`print a Roll object", {
  expect_output(print(newRoll(2, 6)), "2d6", fixed = TRUE)
})
test_that("`print a Roll object", {
  expect_output(print(newRoll(2, 6, 1)), "2d6+1", fixed = TRUE)
})
test_that("`print a Roll object", {
  expect_output(print(newRoll(2, 6, -1)), "2d6-1", fixed = TRUE)
})

#
# EXCEPTIONS
#
test_that("Zero dice throw exception", {
  expect_error(newRoll(0, 6))
})
test_that("1 face throws exception", {
  expect_error(newRoll(2, 1))
})

test_that("Wrong count type throws exception", {
  expect_error(newRoll("2", 6))
})
test_that("Wrong face type throws exception", {
  expect_error(newRoll(2, "6"))
})
test_that("Wrong mod type throws exception", {
  expect_error(newRoll(2, 6, "3"))
})

test_that("Fractional dice throw exception", {
  expect_error(newRoll(2.2, 6))
})
test_that("Fractional face throws exception", {
  expect_error(newRoll(2, 5.9))
})
test_that("Fractional mod throws exception", {
  expect_error(newRoll(2, 6, 1.1))
})
