test_that("HP=0 reflects skill", {
  # When HP-modifier >= 0, the probability of HP=0 should equal the
  # probability of failure, which is (1 - skill/20)
  # Further weapon attributes should not affect this probability,
  # as they only affect the distribution of HP > 0
  for (eav in c(0, 5, 10, 15, 20)) {
    wcount <- sample(1:3, 1)
    wfaces <- sample(2:20, 1)
    wmod <- sample(0:5, 1)
    expect_equal(
      phitpoints(0, eav, c(Count=wcount, Faces=wfaces, Mod=wmod)),
      1 - (eav/20.0) |> setNames(0),
      label = paste("eav = ", eav)
    )
  }
})


test_that("AT4 1W2+1", {
  skill <- 4
  pfail <- (20 - skill) / 20

  expect_equal(
    phitpoints(0:6, skill, c(Count=1, Faces=2, Mod=1)),
    cumsum(c(pfail, 0, 0.095, 0.095, 0.005, 0.0, 0.005)) |>
      setNames(0:6)
  )
})



test_that("AT12 1W3+0", {
  for (skill in c(9, 12, 17)) {
    pcrit <- 0.05 * skill / 20
    psuccess <- skill / 20 - pcrit
    pfail <- 1 - (skill / 20)

    expect_equal(
      phitpoints(1:6, skill, c(Count=1, Faces=3, Mod=0)),
      pfail + c(psuccess/3.0, (psuccess + pcrit) / 3.0,
                psuccess / 3.0, pcrit / 3.0, 0.0, pcrit / 3.0) |>
        cumsum() |>
        setNames(1:6)
    )
  }
})



#
# BOUNDARIES
#
test_that("q = numeric(0) returns numeric(0)", {
  skill <- sample(1:20, 1) # skill should not matter

  expect_equal(
    phitpoints(numeric(0), skill, c(Count=1, Faces=6, Mod=1)),
    numeric()
  )
})

test_that("q = NA returns numeric(0)", {
  skill <- sample(1:20, 1) # skill should not matter

  expect_equal(
    phitpoints(NA, skill, c(Count=1, Faces=6, Mod=1)),
    NA_real_
  )
})

#
# EXCEPTIONS
#

# no exceptions beyond `dhitpoints`


