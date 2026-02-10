# Tests are also done implicitly by testing `dSkill`

test_that("... sum is always 58 (with random input)", {
  expect_equal(
    botch3d20(sample(20, 3, TRUE)) |>
      sum(),
    58
  )
})

test_that("... length is always 60 (with random input)", {
  expect_equal(
    botch3d20(sample(20, 3, TRUE)) |>
      length(),
    60
  )
})
