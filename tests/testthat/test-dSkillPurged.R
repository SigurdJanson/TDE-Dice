#
# EDGE CASES
#
test_that("1/1/1:0 & 1 - matches dsa5.mueller-kalthoff.com", {
  eav <- c(1L, 1L, 1L)
  skill <- c(0L, 1L, 5L, 20L)

  #expected <- c(`Failed` = 0.99275, `0` = 0.00725)
  expected <- d3D20() |> setNames(1:60)
  expected[41:59] <- expected[41:59] - 3/8000 # double twenty
  expected[60]    <- expected[60] - 1/8000 # triple twenty
  expected[4:22]  <- expected[4:22] - 3/8000 # double one
  expected[3]     <- expected[3] - 1/8000  # triple one

  for(s in skill) {
    expect_equal(dSkillPurged(NA, eav, skill), expected,
                 info = paste("skill = ", s, collapse = ", "))
  }
})

test_that("20/20/20:s - matches dsa5.mueller-kalthoff.com", {
  eav <- c(20L, 20L, 20L)
  skill <- c(0L, 1L, 5L, 20L)

  # Result is independent of skill level
  for(s in skill) {
    expected <- c(rep(0, 59), 1 - 2*0.00725) |> setNames(1:60)
    expect_equal(dSkillPurged(NA, eav, s), expected,
                 info = paste("skill = ", s, collapse = ", "))
  }
})
