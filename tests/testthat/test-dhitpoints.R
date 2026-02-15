test_that("AT4 1W2+1", {
  skill <- 4

  expect_equal(
    dhitpoints(1:6, skill, c(Count=1, Dice=2, Mod=1)),
    c(0, 0.095, 0.095, 0.005, 0.0, 0.005) |>
      setNames(1:6)
  )
})

test_that("AT10 1W3+0", {
  skill <- 10
  critchance <- 0.05 * skill / 20
  successchance <- skill / 20 - critchance

  expect_equal(
    dhitpoints(1:6, skill, c(Count=1, Dice=3, Mod=0)),
    c(successchance/3.0,
      (successchance + critchance) / 3.0,
      successchance / 3.0, critchance / 3.0,
      0.0, critchance / 3.0) |>
      setNames(1:6)
  )
})

test_that("AT12 1W6+2", {
  skill <- 12
  critchance <- 0.05 * skill / 20
  successchance <- skill / 20 - critchance

  expect_equal(
    dhitpoints(1:16, skill, c(Count=1, Dice=6, Mod=2)),
    c(0, 0, 0.095, successchance / 6.0, successchance / 6.0,
      (successchance + critchance) / 6.0, successchance / 6.0,
      (successchance + critchance) / 6.0,
      0.0, critchance / 6.0, 0.0, critchance / 6.0, 0.0,
      critchance / 6.0, 0.0, critchance / 6.0) |>
      setNames(1:16)
  )
})


test_that("AT8 1W7+2 - strange die", {
  skill <- 8

  expect_equal(
    dhitpoints(1:18, skill, c(Count=1, Dice=7, Mod=2)),
    c(0, 0, 0.0542857142857143, 0.0542857142857143, 0.0542857142857143,
      0.0571428571428572, 0.0542857142857143, 0.0571428571428572,
      0.0542857142857143, 0.00285714285714286, 0, 0.00285714285714286,
      0, 0.00285714285714286, 0, 0.00285714285714286, 0, 0.00285714285714286) |>
      setNames(1:18)
  )
})


test_that("AT11 2W6+0", {
  skill <- 11

  expect_equal(
    dhitpoints(1:24, skill, c(Count=2, Dice=6, Mod=0)),
    c(0, 0.0145138888888889, 0.0290277777777778, 0.0443055555555556,
      0.0580555555555556, 0.0740972222222222, 0.0870833333333334,
      0.0748611111111111, 0.0580555555555556, 0.0465972222222222,
      0.0290277777777778, 0.0183333333333333, 0.0, 0.00458333333333333,
      0.0, 0.00381944444444444, 0.0, 0.00305555555555556, 0.0,
      0.00229166666666667, 0.0, 0.00152777777777778, 0.0, 0.000763888888888889) |>
      setNames(1:24)
  )
})


test_that("AT19 3W3+4", {
  skill <- 19

  expect_equal(
    dhitpoints(1:26, skill, c(Count=3, Dice=3, Mod=4)),
    c(rep(0, 6), 0.0334259259259259, 0.100277777777778, 0.200555555555556,
      0.233981481481481, 0.200555555555556, 0.100277777777778, 0.0334259259259259,
      0.00175925925925926, 0, 0.00527777777777778, 0, 0.0105555555555556, 0,
      0.0123148148148148, 0, 0.0105555555555556, 0, 0.00527777777777778, 0,
      0.00175925925925926) |>
      setNames(1:26)
  )
})


#
# BOUNDARIES
#
test_that("x = numeric(0) returns numeric(0)", {
  skill <- sample(1:20, 1) # skill should not matter

  expect_equal(
    dhitpoints(numeric(0), skill, c(Count=1, Dice=6, Mod=1)),
    numeric()
  )
})

test_that("eav > 20 returns ", {
  skill <- 21

  expect_no_error(
    result <- dhitpoints(1:6, skill, c(Count=1, Dice=6, Mod=1))
  )
  expect_equal(
    result,
    dhitpoints(1:6, 20L, c(Count=1, Dice=6, Mod=1))
  )
})


#
# EXCEPTIONS
#
test_that("x = 0 i.e. lower than minimum hitpoints throws exception", {
  skill <- sample(1:20, 1) # skill should not matter

  expect_error(
    dhitpoints(0, skill, c(Count=1, Dice=2, Mod=1))
  )
})

test_that("x > maximum hitpoints throws exception", {
  skill <- sample(1:20, 1) # skill should not matter

  expect_error(
    dhitpoints((6+1)*2 + 1L, skill, c(Count=1, Dice=6, Mod=1))
  )
})
