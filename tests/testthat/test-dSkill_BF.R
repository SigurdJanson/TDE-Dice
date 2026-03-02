

# at skill 0 = 12.8750%, 1 = 3.7125%, 2 = 4.0875%,
test_that("10/10/10:2 - matches dsa5.mueller-kalthoff.com", {
  eav <- c(10L, 10L, 10L)
  skill <- 2L

  # order is reversed!!!
  expected <- c(`-x` = 0.79325, `0` = 0.040875, `1` = 0.037125, `2` = 0.12875)

  expect_equal(
    dSkill_BF(eav, skill),
    expected * 8000
  )
})


# at skill 0 = 20.6875, 1 = 26.1, 2 = 31.9750, 3 = 38.3250, 4 = 43.9750, 5 = 49.8750
# c(20.6875, 26.1, 31.9750, 38.3250, 43.9750, 49.8750)
test_that("10/10/10:2 - matches dsa5.mueller-kalthoff.com", {
  eav <- c(12L, 8L, 17L)
  skill <- 5L

  # Note: order is reversed!!! Every additional skill point is added to the left
  expected <- c(`-x` = 0.50125, `0` = 0.059, `1` = 0.0565, `2` = 0.0635, `3` = 0.05875, `4` = 0.054125, `5` = 0.206875)

  expect_equal(
    dSkill_BF(eav, skill),
    expected * 8000
  )
})


# SP(8) = 57.2875, SP(7) = 51.325, SP(6) = 45.6125, SP(5) = 40.15,
# SP(4) = 34.9375, SP(3) = 29.975, SP(2) = 25.2625, SP(1) = 20.8, SP(0) = 15.975
test_that("19/11/16:8 - matches dsa5.mueller-kalthoff.com", {
  eav <- c(19L, 11L, 6L)
  skill <- 8L

  # Note: order is reversed!!! Every additional skill point is added to the left
  expected <- c(`-x` = 0.427125, `0` = 0.059625, `1` = 0.057125, `2` = 0.054625,
                `3` = 0.052125, `4` = 0.049625, `5` = 0.047125,
                `6` = 0.044625, `7` = 0.04825, `8` = 0.15975)

  expect_equal(
    dSkill_BF(eav, skill),
    expected * 8000
  )
})



test_that("1/1/1:0 & 1 - matches dsa5.mueller-kalthoff.com", {
  eav <- c(1L, 1L, 1L)
  skill <- 0L

  expected <- c(`-x` = 0.99275, `0` = 0.00725)
  expect_equal(dSkill_BF(eav, skill), expected * 8000)

  skill <- 1L
  expected <- c(`-x` = 0.99275, `1` = 0.00725)
  expect_equal(dSkill_BF(eav, skill), expected * 8000)
})

test_that("20/20/20:0 - matches dsa5.mueller-kalthoff.com", {
  eav <- c(20L, 20L, 20L)
  skill <- c(0L, 1L, 5L, 20L)

  for(s in skill) {
    expected <- c(0.00725, 0.99275) |> setNames(c("-x", s))
    expect_equal(dSkill_BF(eav, s), expected * 8000)
  }
})



#
# EXCEPTIONS
#
test_that("eav <= 0 returns error", {
  eav <- sample(c(10L, 0L, 10L))
  skill <- 2L

  expect_error(dSkill_BF(eav, skill))
})
test_that("length(eav) != 3 returns error", {
  eav <- c(10L, 10L, 10L, 10L)
  skill <- 2L

  expect_error(dSkill_BF(eav, skill))
})
test_that("skill < 0 returns error", {
  eav <- sample(c(10L, 10L, 10L))
  skill <- -1L

  expect_error(dSkill_BF(eav, skill))
})
