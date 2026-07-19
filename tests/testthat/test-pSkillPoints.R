




#
# EXCEPTIONS
#
test_that("x < -1 returns error", {
  eav <- sample(c(10L, 10L, 10L))
  skill <- 2L
  x <- sample(c(-1:8, -2))

  expect_error(pSkillPoints(x, eav, skill))
})
test_that("eav <= 0 returns error", {
  eav <- sample(c(10L, 0L, 10L))
  skill <- 2L

  expect_error(pSkillPoints(0:8, eav, skill))
})
test_that("length(eav) != 3 returns error", {
  eav <- c(10L, 10L, 10L, 10L)
  skill <- 2L

  expect_error(pSkillPoints(0:8, eav, skill))
})
test_that("skill < 0 returns error", {
  eav <- sample(c(10L, 10L, 10L))
  skill <- -1L

  expect_error(pSkillPoints(0:8, eav, skill))
})
