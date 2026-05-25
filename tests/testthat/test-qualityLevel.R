test_that("values below 0 yield 0 - input type is integer", {
  sp <- -1L:20L
  expect_equal(
    qualityLevel(sp),
    c(0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6)
  )
})


test_that("values below 0 yield 0 - input type is numeric", {
  sp <- -1L:20L |> as.numeric()
  expect_equal(
    qualityLevel(sp),
    c(0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6)
  )
})


test_that("Ordered factor as input (created by 'dSkillPoints')", {
  sp <- dSkillPoints(-1L:20L, c(7L, 10L, 13L), 20L, "df")$Remainder
  expect_equal(
    qualityLevel(sp),
    c(0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6)
  )
})



test_that("values below 0 yield 0", {
  sp <- -1L:-20
  expect_equal(
    qualityLevel(sp),
    rep(0L, length(sp))
  )
})



#
# EXCEPTIONS - NOT IMLPEMENTED BECAUSE FUNCTION IS NOT INTENDED TO BE EXPORTED
#

test_that("NULL throws an error (because it is an unknown class)", {
  expect_error(qualityLevel(NULL))
})

# test_that("NA returns NA", {
#   expect_equal(
#     qualityLevel(NA),
#     NA
#   )
# })
