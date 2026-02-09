test_that("values below 0 yield 0", {
  sp <- 0:20
  expect_equal(
    qualityLevel(sp),
    c(1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6)
  )
})


test_that("values below 0 yield 0", {
  sp <- -1L:-20
  expect_equal(
    qualityLevel(sp),
    rep(0L, length(sp))
  )
})


# test_that("NULL returns NA", {
#   expect_equal(
#     qualityLevel(NULL),
#     NA
#   )
# })
#
# test_that("NA returns NA", {
#   expect_equal(
#     qualityLevel(NA),
#     NA
#   )
# })
