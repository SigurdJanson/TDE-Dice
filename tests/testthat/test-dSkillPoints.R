#
# SPECIFIC CASES ####
#
# 2 = 20.6750, 1 = 16.5875, 12.8750
test_that("10/10/10, SP=2 - ", {
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
test_that("5/10/15, SP=5 - ", {
  eav <- c(5L, 10L, 15L)
  skill <- 5L

  expected <- c(0.69375, 0.04975, 0.045625, 0.041625, 0.03775, 0.034, 0.0975) |>
    setNames(c("Failed", 0:skill))

  expect_equal(
    dSkillPoints(-1:skill, eav, skill),
    expected
  )
})

# `1` = 50, `0` = 50
test_that("10/20/20, SP=0 - gives 50% probability", {
  eav <- sample(c(10L, 20L, 20L), 3L)
  skill <- 0L

  expected <- c(0.5, 0.5) |>
    setNames(c("Failed", 0:skill))

  expect_equal(
    dSkillPoints(-1:skill, eav, skill),
    expected
  )
})


#
# EDGE CASES ####
#
test_that("1/1/1, SP=0 & SP=1 - matches dsa5.mueller-kalthoff.com", {
  eav <- c(1L, 1L, 1L)
  skill <- 0L

  expected <- c(`Failed` = 0.99275, `0` = 0.00725)
  expect_equal(dSkillPoints(c(-1, 0), eav, skill), expected)

  skill <- 1L
  expected <- c(`Failed` = 0.99275, `0` = 0, `1` = 0.00725)
  expect_equal(dSkillPoints(c(-1, 0, 1), eav, skill), expected)

  skill <- 20L
  expected <- c(`Failed` = 6232, `0` = 231, `1` = 210, `2` = 190,
                `3` = 171, `4` = 153, `5` = 136, `6` = 120,
                `7` = 105, `8` = 91, `9` = 78, `10` = 66,
                `11` = 55, `12` = 45, `13` = 36, `14` = 28,
                `15` = 21, `16` = 15, `17` = 10, `18` = 6,
                `19` = 3, `20` = 1) / 8000
  print(sum(expected))
  expect_equal(dSkillPoints(c(-1, 0:20), eav, skill), expected)

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
# PLAUSBILITY ####
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
# FORMAT CONSISTENCY ####
#
test_that("dSkillPoints format consistency and normalization", {
  # Setup test parameters
  test_x <- -1:5
  eav <- c(10, 10, 10)
  skill <- 10

  # Get both formats
  df_out <- dSkillPoints(test_x, eav, skill, format = "df")
  vec_out <- dSkillPoints(test_x, eav, skill, format = "vector")

  # 1. Check dimensions
  expect_length(vec_out, length(test_x))
  expect_equal(nrow(df_out), length(test_x))

  # 2. Check that both formats yield identical values
  # The vector format uses labels, so we compare the numeric values
  expect_equal(as.numeric(vec_out), df_out$p)

  # 3. Check for valid probability properties
  # Probabilities must be between 0 and 1
  expect_true(all(df_out$p >= 0))
  expect_true(all(df_out$p <= 1))

  # 4. Check label naming for vector format
  # "Failed" should be the first name
  expect_equal(names(vec_out)[1], "Failed")
  # Subsequent names should match the numeric input
  expect_equal(names(vec_out)[-1], as.character(test_x[-1]))
})

test_that("dSkillPoints handles edge cases for 'x'", {
  # Ensure requested 'x' values that fall outside natural range return 0
  # rather than failing or returning NAs
  extreme_x <- c(-1, 100)
  result <- dSkillPoints(extreme_x, c(10, 10, 10), 10, format = "df")

  expect_equal(result$p[result$Remainder == 100], 0)
  expect_equal(nrow(result), 2)
})


#
# EXCEPTIONS ####
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
