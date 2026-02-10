#
# PAIR OF FAIR DICE ####
#

test_that("3d20", {
expected <- c(0, 0, 1, 3, 6, 10, 15, 21, 28,
              36, 45, 55, 66, 78, 91, 105, 120, 136, 153,
              171, 190, 210, 228, 244, 258, 270, 280, 288, 294,
              298, 300, 300, 298, 294, 288, 280, 270, 258, 244,
              228, 210, 190, 171, 153, 136, 120, 105, 91, 78,
              66, 55, 45, 36, 28, 21, 15, 10, 6, 3, 1)
  expect_equal(
    convolveDice(rep(1, 20), rep(1, 20)) |>
      convolveDice(rep(1, 20)),
    expected
  )
})

test_that("2d6", {
  expected <- c(0, 1, 2, 3, 4, 5,
                6, 5, 4, 3, 2, 1)
  expect_equal(
    convolveDice(rep(1, 6), rep(1, 6)),
    expected
  )
})


test_that("2d3", {
  expected <- c(0, 1, 2, 3, 2, 1)
  expect_equal(
    convolveDice(rep(1, 3), rep(1, 3)),
    expected
  )
})


#
# 1 is TRUNCATED RECTIFIED
#

test_that("d6 X d[3, 3, 3, 4, 5, 6]", {
  expected <- c(0, 0, 0, 3, 4, 5,
                6, 6, 6, 3, 2, 1)
  expect_equal(
    convolveDice(
      c(0, 0, 3, rep(1, 3)),
      rep(1, 6)),
    expected
  )
})

