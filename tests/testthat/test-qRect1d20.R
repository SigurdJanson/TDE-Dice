
test_that("p of 5/20 returns 5", {
  expect_equal(qRect1d20(0.25, 1), 5L)
})

test_that("p > 13/20 returns 14", {
  expect_equal(qRect1d20(0.66, 1), 14L)
})


test_that("p = seq(0.20-0.80) with eav 12 returns c(rep(12, 9), 13:16)", {
  expect_equal(qRect1d20(seq(0.20, 0.80, 0.05), 12), c(rep(12, 9), 13:16))
})


test_that("probabilities below `eav` return `eav`", {
  eav <- 5L
  for (p in 1:eav/20)
    expect_equal(qRect1d20(p, eav), eav)
  for (p in (eav+1):20/20)
    expect_equal(qRect1d20(p, eav), p*20)
})
test_that("probability below `eav` returns eav, then it grows", {
  expect_equal(qRect1d20(0.66, 14L), 14L)
  expect_equal(qRect1d20(0.66, 15L), 15L)
})

#
# Boundary values
#
test_that("p=0 returns eav", {
  for(eav in c(1, 10, 20))
    expect_equal(qRect1d20(0, eav), eav)
})
test_that("A very small p of 5/20 returns eav", {
  for(eav in c(1, 10, 20))
    expect_equal(qRect1d20(1E-10, eav), eav)
})

test_that("p=1 returns 20 independent of `eav`", {
  for(eav in c(1, 10, 20))
    expect_equal(qRect1d20(1, eav), 20)
})

#
# ARGUMENT lower.trail
#
test_that("p of 5/20=0.25 and lower-trail=F returns 15", {
  expect_equal(qRect1d20(0.25, 1, lower.tail = FALSE), 15L)
})
test_that("p of 0.22 and lower-trail=F returns 16", {
  expect_equal(qRect1d20(0.22, 1, lower.tail = FALSE), 16L)
})
test_that("p > 13/20 and eav=14 returns the eav", {
  expect_equal(qRect1d20(0.66, 1), 14L)
})


#
# EXCEPTIONS
#
test_that("length(eav) > 1 throws exception", {
  expect_error(qRect1d20(1:20, 1:2L))
})
test_that("length(eav) < 1 throws exception", {
  expect_error(qRect1d20(1:20, numeric()))
})

test_that("eav = 0 throws exception", {
  expect_error(qRect1d20(1:20, 0))
})
test_that("eav = 21 throws exception", {
  expect_error(qRect1d20(1:20, 21))
})

test_that("eav = NULL throws exception", {
  expect_error(qRect1d20(1:20, NULL))
})
test_that("eav = NA throws exception", {
  expect_error(qRect1d20(5, NA))
})


test_that("p = NULL throws exception", {
  expect_error(qRect1d20(NULL, 2))
})
test_that("p = NA throws exception", {
  expect_error(qRect1d20(NA, 2))
})
