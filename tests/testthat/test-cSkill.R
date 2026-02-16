#
# SPECIFIC CASES
#
test_that("10/10/10:2 - ", {
  expected <- list(
    Critical = 0.00725,
    Success = 0.1995,
    Fail = 0.786,
    Botch = 0.00725
  ) |> structure(class = "CSFB", check = "Skill")

  expect_equal(
    cSkill(c(10, 10, 10), 2),
    expected
  )
})


test_that("10/11/12:5 - ", {
  expected <- list(
    Critical = 0.00725,
    Success = 0.428,
    Fail = 0.5575,
    Botch = 0.00725
  ) |> structure(class = "CSFB", check = "Skill")

  expect_equal(
    cSkill(c(10, 11, 12), 5),
    expected
  )
})


test_that("2/10/18:9 - ", {
  expected <- list(
    Critical = 0.00725,
    Success = 0.396625,
    Fail = 0.588875,
    Botch = 0.00725
  ) |> structure(class = "CSFB", check = "Skill")

  expect_equal(
    cSkill(c(2, 10, 18), 9),
    expected
  )
})


test_that("19+5/6+5/14+5:4 - ", {
  expected <- list(
    Critical = 0.00725,
    Success = 0.739125,
    Fail = 0.246375,
    Botch = 0.00725
  ) |> structure(class = "CSFB", check = "Skill")

  expect_equal(
    cSkill(c(19, 6, 14) + 5, 4),
    expected
  )
})


test_that("1/5/5:1 - ", {
  expected <- list(
    Critical = 0.00725,
    Success = 0.006,
    Fail = 0.9795,
    Botch = 0.00725
  ) |> structure(class = "CSFB", check = "Skill")

  expect_equal(
    cSkill(c(1, 5, 5), 1),
    expected
  )
})


test_that("6/16/18:18 - ", {
  expected <- list(
    Critical = 0.00725,
    Success = 0.9855,
    Fail = 0.0,
    Botch = 0.00725
  ) |> structure(class = "CSFB", check = "Skill")

  expect_equal(
    cSkill(c(6, 16, 18), 18),
    expected
  )
})


test_that("12/19/19:13 - ", {
  expected <- list(
    Critical = 0.00725,
    Success = 0.9855,
    Fail = 0.0,
    Botch = 0.00725
  ) |> structure(class = "CSFB", check = "Skill")

  expect_equal(
    cSkill(c(12, 19, 19), 13),
    expected
  )
})



test_that("7/10/13:8 - ", {
  expected <- list(
    Critical = 0.00725,
    Success = 0.501625,
    Fail = 0.483875,
    Botch = 0.00725
  ) |> structure(class = "CSFB", check = "Skill")

  expect_equal(
    cSkill(c(7, 10, 13), 8),
    expected
  )
  expect_equal(
    cSkill(c(7, 10, 13), 8) |>
      unlist() |> sum(),
    1.0
  )
})



#
# PLAUSIBILITY TESTS
#
test_that("sum is always 1", {
  eav <- sample(20, 3L, replace = TRUE)
  skill <- sample(10, 1L)
  expect_equal(
    cSkill(eav, skill) |> unlist() |> sum(),
    1.0
  )
})
