test_that("`newCSFB` returns object", {
  expect_s3_class(newCSFB(0.05, 0.45, 0.45, 0.05), "CSFB")
})

test_that("`newCSFB` returns object", {
  expect_equal(
    newCSFB(0.05, 0.45, 0.45, 0.05, "Atta"),
    structure(
      list(Critical = 0.05, Success = 0.45,
           Fail = 0.45, Botch = 0.05),
      class = c("CSFB"),
      check = "Attack"
  ))
})


#
# IS
#
test_that("`is.CSFB()` confirms that newRoll returns a Dice object", {
  expect_true(is.CSFB(newCSFB(0.05, 0.45, 0.45, 0.05, "Dodge")))
})
test_that("`is.CSFB()` confirms that newRoll returns a Dice object", {
  expect_false(is.CSFB(list(2, 6, 4, 5)))
})
test_that("`is.CSFB()` confirms that newRoll returns a Dice object", {
  Malformed <- list(0.05, 0.45, 0.45, 0.05)
  class(Malformed) <- "CSFB"
  attr(Malformed, "check") <- "Some nonsense"
  expect_false(is.CSFB(Malformed))
})


#
# PRINT
#
test_that("print a CSFB object", {
  obj <- newCSFB(0.05, 0.45, 0.45, 0.05, "D")
  expect_output(print.CSFB(obj))
  #-print.CSFB(obj)
})
test_that("print a CSFB object with sum(p) != 1 gives extra comment", {
  obj <- newCSFB(0.05, 0.45, 0.45, 0.04, "P")
  expect_output(print.CSFB(obj), ".*Object is irregular.*")
  #-print.CSFB(obj)
})
test_that("print a CSFB object with many lines gets truncated", {
  obj <- newCSFB(rep(0.05, 50), rep(0.45, 50), rep(0.44, 50), rep(0.05, 50), "P")
  expect_output(print.CSFB(obj))
  #print.CSFB(obj)
})



