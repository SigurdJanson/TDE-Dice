

test_that("Works with .qlfactor", {
  qlf <- .qlfactor(sample(0:6, 20, replace = TRUE))
  expectedlevels <- levels(qlf)

  expect_no_error(inc.ordered(qlf))

  expect_equal(levels(inc.ordered(qlf)), expectedlevels)
})


test_that("Does not increase levels at maximum", {
  qlf <- .qlfactor(rep(6, 5))
  expect_equal(levels(inc.ordered(qlf)), c("Failed", paste0("QL", 1:6)))
})


#
# EXCEPTIONS
#
test_that("Type must be an ordered factor", {
  # No factor
  expect_error(inc.ordered(1:3))
  # Factor but unordered
  expect_error(inc.ordered(factor(1:3, ordered=FALSE)))
  # Correct:
  expect_no_error(inc.ordered(ordered(1:3)))
})
