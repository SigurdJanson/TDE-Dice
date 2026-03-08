#
# SPECIFIC CASES
#
# 2 = 20.6750, 1 = 16.5875, 12.8750
test_that("10/10/10:2 - ", {
  eav <- c(10L, 10L, 10L)
  skill <- 2L

  expected <- c(0.79325, 0.040875, 0.037125, 0.12875, rep(0, 6L)) |>
    setNames(c("Failed", 0:8))

  expect_equal(
    dSkillPoints(-1:8, eav, skill),
    expected
  )
})


# `5` = 30.625, `4` = 25.65, `3` = 21.0875, `2` = 16.925, `1` = 13.15, `0` = 9.75
test_that("5/10/15:5 - ", {
  eav <- c(5L, 10L, 15L)
  skill <- 5L

  expected <- c(0.69375, 0.04975, 0.045625, 0.041625, 0.03775, 0.034, 0.0975) |>
    setNames(c("Failed", 0:skill))

  expect_equal(
    dSkillPoints(-1:skill, eav, skill),
    expected
  )
})



#
# EDGE CASES
#
test_that("1/1/1:0 & 1 - matches dsa5.mueller-kalthoff.com", {
  eav <- c(1L, 1L, 1L)
  skill <- 0L

  expected <- c(`Failed` = 0.99275, `0` = 0.00725)
  expect_equal(dSkillPoints(c(-1, 0), eav, skill), expected)

  skill <- 1L
  expected <- c(`Failed` = 0.99275, `0` = 0, `1` = 0.00725)
  expect_equal(dSkillPoints(c(-1, 0, 1), eav, skill), expected)
})

test_that("20/20/20:s - matches dsa5.mueller-kalthoff.com", {
  eav <- c(20L, 20L, 20L)
  skill <- c(0L, 1L, 5L, 20L)

  for(s in skill) {
    expected <- c(0.00725, rep(0, s), 0.99275) |> setNames(c("Failed", 0:s))
    expect_equal(dSkillPoints(-1:s, eav, s), expected,
                 info = paste("skill = ", s, collapse = ", "))
  }
})



#
# PLAUSBILITY
#
test_that("dSkill: Brute force gives same result as efficient algorithm", {
  eav <- sample(1L:20, 3L, replace = TRUE)
  skill <- sample(0L:20, 1L)

  expected <- dSkill_BF(eav, skill) / 8000 #|> setNames()

  expect_equal(
    dSkillPoints(-1:skill, eav, skill),
    expected,
    info = paste("eav = ", eav, collapse = ", ")
  )
})

test_that("sum is always 1", {
  eav <- sample(20, 3L, replace = TRUE)
  skill <- sample(20, 1L)
  expect_equal(
    dSkillPoints(-1:skill, eav, skill) |> sum(),
    1.0
  )
})



#
# EXCEPTIONS
#
test_that("x < -1 returns error", {
  eav <- sample(c(10L, 10L, 10L))
  skill <- 2L
  x <- sample(c(-1:8, -2))

  expect_error(dSkillPoints(x, eav, skill))
})
test_that("eav <= 0 returns error", {
  eav <- sample(c(10L, 0L, 10L))
  skill <- 2L

  expect_error(dSkillPoints(0:8, eav, skill))
})
test_that("length(eav) != 3 returns error", {
  eav <- c(10L, 10L, 10L, 10L)
  skill <- 2L

  expect_error(dSkillPoints(0:8, eav, skill))
})
test_that("skill < 0 returns error", {
  eav <- sample(c(10L, 10L, 10L))
  skill <- -1L

  expect_error(dSkillPoints(0:8, eav, skill))
})
