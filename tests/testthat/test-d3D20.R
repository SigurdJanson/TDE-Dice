test_that("missing parameter returns the whole distribution", {
  expect_length(d3D20(), length(1:60))
  expect_equal(sum(d3D20()), 1)
})

test_that("NULL parameter returns an empty set", {
  result <- d3D20(NULL)
  expect_length(result, 0L)
  expect_type(result, "double")
})

test_that("known values give the correct result", {
  sums <- c(3, 8, 17, 25, 30)
  # taken from https://www.omnicalculator.com/statistics/dice
  expected <- c(0.000125, 0.002625, 0.015, 0.03225, 0.03725)

  expect_equal(d3D20(sums), expected)
})

test_that("Distribution is symmetric", {
  front <- 3:31
  back  <- 32:60

  result <- d3D20()
  expect_equal(result[front], rev(result[back]))
})
