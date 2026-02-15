
test_that("result is correctly formatted object", {
  result <- cCombat(1:21)
  expect_s3_class(result, "CSFB")
  expect_type(result, "list")
  expect_named(result, c("Critical", "Success", "Fail", "Botch", "Type"))
})


test_that("result is correctly formatted object", {
  result <- cCombat(1:21)
  expect_s3_class(result, "CSFB")
  expect_type(result, "list")
  expect_named(result, c("Critical", "Success", "Fail", "Botch", "Type"))
  for (element in result[1:4]) {
    expect_vector(element, numeric())
  }
  expect_vector(result$Type, character())
})



test_that("sum of values is 1", {
  result <- cCombat(1:21)
  result <- sapply(1:21, \(x) sum(result[[1]][x],
                                  result[[2]][x],
                                  result[[3]][x],
                                  result[[4]][x]))
  expect_all_equal(result, 1)
})


#
# BOUNDARIES
#
test_that("eav=1", {
  result <- cCombat(1)
  expect_equal(
    result,
    structure(
      list(
        Critical = 1/400, Success = 19/400,
        Fail = 19/20-19/400, Botch = 19/400,
        Type = "Attack"
      ), class = "CSFB")
  )
})


test_that("eav=20", {
  result <- cCombat(20)
  expect_equal(
    result,
    structure(
      list(
        Critical = 1/20, Success = 19/20-1/400,
        Fail = 0, Botch = 1/400,
        Type = "Attack"
      ), class = "CSFB")
  )
})


#
# EXCEPTIONS
#
test_that("eav=0 throws error", {
  expect_error(cCombat(0), "'eav' must be >= 1", fixed = TRUE)
})
