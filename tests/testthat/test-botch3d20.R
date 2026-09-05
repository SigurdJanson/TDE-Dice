# Tests are also done implicitly by testing `dSkill`

test_that("... sum is always 58 (with random input)", {
  expect_equal(
    botch3d20(eav = sample(20, 3, TRUE)) |>
      sum(),
    58
  )
})

test_that("... length is always 60 (with random input)", {
  expect_equal(
    botch3d20(eav = sample(20, 3, TRUE)) |>
      length(),
    60
  )
})

test_that("x = 60 is always 1", {
  expect_equal(
    botch3d20(eav = sample(20, 3, TRUE))[60L], 1L
  )

  # test subsetting
  eav = sample(20, 3, TRUE)
  expect_equal(
    botch3d20(eav = eav)[60L], botch3d20(60L, eav = eav)
  )
})
