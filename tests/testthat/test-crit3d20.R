# Tests are also done implicitly by testing `dSkill`

test_that("... sum is always 58 (with random input)", {
  expect_equal(
    crit3d20(sample(20, 3, TRUE)) |>
      sum(),
    58
  )
})

test_that("... length is always 60 (with random input)", {
  expect_equal(
    crit3d20(sample(20, 3, TRUE)) |>
      length(),
    60
  )
})



# test_that("crit3d20(5, 10, 15)", {
#   expected <- c(
#     rep(0, 2), # skip the 1 and 2
#
#   )
#   expect_equal(
#     crit3d20(5, 10, 15),
#     expected
#   )
# })
