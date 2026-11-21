


test_that("skill = 10 gives 50% chance", {
  dice <- c(Count=1L, Faces=6L, Mod=0L)
  skill <- 10
  pcrit <- (1/20 * skill/20)

  result <- qhitpoints(0.5, skill, dice)

  # with skill = 10, 50% are misses
  expect_equal(qhitpoints(0.5, skill, dice), 0L)

  # Asking for p > 0.5 returns "1 hit point" = 1L
  expect_equal(
    qhitpoints(0.5+.Machine$double.eps, skill, dice),
    1L)
  # Asking for p being slightly below p(fail) > 0.5 returns "zero hit points" = 0L
  expect_equal(
    qhitpoints(0.5-.Machine$double.eps, skill, dice),
    0L)

  # Next border between hp = 1 and hp = 2
  # Asking for p > 0.5+1/6 still returns "1 hitpoint" = 1L
  pThreshold <- 0.5 + ((0.5-pcrit) * 1/6)
  expect_equal(
    qhitpoints(pThreshold, skill, dice),
    1L)
  expect_equal(
    qhitpoints(pThreshold-.Machine$double.eps, skill, dice),
    1L)
  # Asking for something a tiny bit larger returns "2 hitpoints" = 2L
  expect_equal(
    qhitpoints(pThreshold+.Machine$double.eps, skill, dice),
    2L)
})



#
# BOUNDARIES
#
test_that("x = numeric(0) returns numeric(0)", {
  skill <- sample(1:20, 1) # skill should not matter

  expect_equal(
    qhitpoints(numeric(0), skill, c(Count=1, Faces=6, Mod=1)),
    numeric()
  )
})

test_that("x = NA returns NA", {
  skill <- sample(1:20, 1) # skill should not matter
  weapon <- c(Count=1, Faces=6, Mod=1)

  expect_equal(
    qhitpoints(NA, skill, weapon),
    NA_integer_
  )

  p <- c(0.25, NA, 0.75, NA, 1)
  expect_equal(
    qhitpoints(p, skill, weapon),
    c(qhitpoints(0.25, skill, weapon), NA_integer_,
      qhitpoints(0.75, skill, weapon), NA_integer_,
      qhitpoints(1, skill, weapon))
  )
})


test_that("eav > 20 returns same as eav = 20", {
  eav <- 21
  weapon <- c(Count=1, Faces=6, Mod=1)

  expect_no_error(
    result <- qhitpoints(seq(0, 1, 0.125), eav, weapon)
  )
  expect_equal(
    result,
    qhitpoints(seq(0, 1, 0.125), 20L, weapon)
  )
})


#
# EXCEPTIONS
#
test_that("p out of bounds throws an error", {
  eav <- 21
  weapon <- c(Count=1, Faces=6, Mod=3)

  expect_no_error(
    qhitpoints(seq(0.0, 1.0, 0.125), eav, weapon)
  )
  expect_error(
    qhitpoints(seq(0, 1, 0.125) + .Machine$double.eps, eav, weapon)
  )
  expect_error(
    qhitpoints(seq(0, 1, 0.125) - .Machine$double.eps, eav, weapon)
  )
})


