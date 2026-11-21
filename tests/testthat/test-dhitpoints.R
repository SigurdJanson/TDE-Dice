test_that("AT4 1W2+1", {
  skill <- 4

  expect_equal(
    dhitpoints(1:6, skill, c(Count=1, Faces=2, Mod=1)),
    c(0, 0.095, 0.095, 0.005, 0.0, 0.005) |>
      setNames(1:6)
  )
})

test_that("AT10 1W3+0", {
  skill <- 10
  critchance <- 0.05 * skill / 20
  successchance <- skill / 20 - critchance

  expect_equal(
    dhitpoints(1:6, skill, c(Count=1, Faces=3, Mod=0)),
    c(successchance/3.0,
      (successchance + critchance) / 3.0,
      successchance / 3.0, critchance / 3.0,
      0.0, critchance / 3.0) |>
      setNames(1:6)
  )
})


test_that("AT10 1W6+0", {
  skill <- 10
  expected <- c(0.5, 0.475 * rep(1/6, 6L), rep(0, 6L)) +
    c(0, 0, 0.025, 0, 0.025, 0, 0.025, 0, 0.025, 0, 0.025, 0, 0.025) * 1/6

  expect_equal(
    dhitpoints(0:12, skill, c(Count=1, Faces=6, Mod=0)),
    expected |>
      setNames(0:12)
  )
})

test_that("AT15 1W6+0", {
  skill <- 15
  expected <- c(0.25, 0.11875, 0.125, 0.11875, 0.125, 0.11875, 0.125, 0, 0.0062500000000000, 0, 0.0062500000000000, 0, 0.0062500000000000)

  expect_equal(
    dhitpoints(0:12, skill, c(Count=1, Faces=6, Mod=0)),
    expected |>
      setNames(0:12)
  )
})

test_that("AT10 1W6+1/-1 - modifiers work", {
  skill <- 10

  ## 1W+1
  expected <- c(0.5, 0, 0.475 * rep(1/6, 6L), rep(0, 7L)) +
    c(0, 0, 0, 0, 0.025, 0, 0.025, 0, 0.025, 0, 0.025, 0, 0.025, 0, 0.025) * 1/6

  expect_equal(
    dhitpoints(0:14, skill, c(Count=1, Faces=6, Mod=1)),
    expected |>
      setNames(0:14)
  )

  ## 1W-1
  expected <- c((0.5 + 0.475 * 1/6), 0.475 * rep(1/6, 5L), rep(0, 9L)) +
    c(0, 0.025, 0, 0.025, 0, 0.025, 0, 0.025, 0, 0.025, 0, 0.025, 0, 0, 0) * 1/6
  expected <- c(0.583333333333334 ,0.0791666666666668 ,0.0833333333333335 ,0.0791666666666668 ,0.0833333333333335 ,0.0791666666666668 ,0.00416666666666668 ,0 ,0.00416666666666668 ,0 ,0.00416666666666668 ,0 ,0 ,0 ,0)
  expect_equal(
    dhitpoints(0:14, skill, c(Count=1, Faces=6, Mod=-1)),
    expected |>
      setNames(0:14)
  )

})



test_that("AT12 1W6+2", {
  skill <- 12
  critchance <- 0.05 * skill / 20
  successchance <- skill / 20 - critchance

  expect_equal(
    dhitpoints(1:16, skill, c(Count=1, Faces=6, Mod=2)),
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
    dhitpoints(1:18, skill, c(Count=1, Faces=7, Mod=2)),
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
    dhitpoints(1:24, skill, c(Count=2, Faces=6, Mod=0)),
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
    dhitpoints(1:26, skill, c(Count=3, Faces=3, Mod=4)),
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
    dhitpoints(numeric(0), skill, c(Count=1, Faces=6, Mod=1)),
    numeric()
  )
})

test_that("x = NA returns NA", {
  skill <- sample(1:20, 1) # skill should not matter

  expect_equal(
    dhitpoints(NA, skill, c(Count=1, Faces=6, Mod=1)),
    NA_real_
  )
})


test_that("eav > 20 returns same as eav = 20", {
  skill <- 21

  expect_no_error(
    result <- dhitpoints(1:6, skill, c(Count=1, Faces=6, Mod=1))
  )
  expect_equal(
    result,
    dhitpoints(1:6, 20L, c(Count=1, Faces=6, Mod=1))
  )
})


test_that("modifier that causes hit points of 0 and lower gives correct results", {
  skill <- 10
  result <- dhitpoints(0:6, skill, c(Count=1, Faces=6, Mod=-5))
  pcrit <- (1/20 * 10/20)
  p0 <- 0.5 + (0.5 * 5/6) # 50% are 0 hp because of failure; 5/6 of the success also

  expect_equal(sum(result), 1.000)
  expect_equal(
    result,
    c(p0, 0.5 * 1/6 - (pcrit * 1/6), pcrit * 1/6, rep(0.0, 4L)) |>
      setNames(0:6)
  )
})




#
# EXCEPTIONS
#
test_that("x = 0 i.e. lower than minimum hitpoints is valid", {
  skill <- sample(1:20, 1) # skill should not matter

  expect_no_error(
    result <- dhitpoints(0, skill, c(Count=1, Faces=2, Mod=1))
  )
})

test_that("x > maximum hitpoints is valid", {
  skill <- sample(1:20, 1) # skill should not matter

  expect_no_error(
    result <- dhitpoints((6+1)*2 + 1L, skill, c(Count=1, Faces=6, Mod=1))
  )
  expect_equal(result, c(`15` = 0.0))
})
