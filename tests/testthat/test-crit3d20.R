# Tests are also done implicitly by testing `dSkill`

test_that("... sum is always 58 (with random input)", {
  expect_equal(
    crit3d20(eav = sample(20, 3, TRUE)) |>
      sum(),
    58
  )
})

test_that("... length is always 60 (with random input)", {
  expect_equal(
    crit3d20(eav = sample(20, 3, TRUE)) |>
      length(),
    60
  )
})


test_that("x = lowest is always sum-2", {
  eav = sample(20, 3, TRUE)
  expect_equal(
    crit3d20(eav = eav)[sum(eav)], sum(eav) - 2L
  )

  # test subsetting
  expect_equal(
    crit3d20(eav = eav)[sum(eav)], crit3d20(sum(eav), eav = eav)
  )
})

