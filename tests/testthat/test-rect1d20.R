test_that("eav = 1 vector with 20 ones", {
  expect_equal(
    rect1d20(1L),
    rep(1, 20)
  )
})

test_that("eav = 2 returns correct result", {
  expect_equal(
    rect1d20(2L),
    c(0, 2, rep(1, 18))
  )
})

test_that("eav = 10 returns correct result", {
  expect_equal(
    rect1d20(10L),
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  )
})

test_that("eav = 19 returns correct result", {
  expect_equal(
    rect1d20(19L),
    c(rep(0, 18), 19, 1)
  )
})

test_that("eav = 20 returns correct result", {
  expect_equal(
    rect1d20(20L),
    c(rep(0, 19), 20)
  )
})
