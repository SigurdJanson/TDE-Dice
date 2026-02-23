test_that("cAttr: eav = 10", {
  eav <- 10L
  expect_equal(
    cAttr(eav),
    structure(
      list( # test checked, 23.2.2026
        Critical = 0.5 / 20,
        Success = 9.0 / 20 + 0.5 / 20,
        Fail = 9.0 / 20 + 0.5 / 20,
        Botch = 0.5 / 20),
      class = "CSFB", check = "Attribute"
    ),
    info = paste("eav = ", eav)
  )
})


test_that("cAttr: eav = c(3, 7, 13)", {
  eav <- c(3, 7, 13)
  expect_equal(
    cAttr(eav),
    structure(
      list( # test checked, 23.2.2026
        Critical = eav / 400,
        Success = eav * 19 / 400,
        Fail = (380 - (19*eav)) / 400,
        Botch = (20 - eav) / 400),
      class = "CSFB", check = "Attribute"
    ),
    info = paste("eav = ", eav)
  )
})


#
# BOUNDARIES
#
test_that("cAttr: eav = 1", {
  eav <- 1L
  expect_equal(
    cAttr(eav),
    structure(
      list( # test checked, 23.2.2026
        Critical = 1 / 400,
        Success = 19/400,
        Fail = (380-19) / 400,
        Botch = 19/400),
      class = "CSFB", check = "Attribute"
    ),
    info = paste("eav = ", eav)
  )
})

test_that("cAttr: eav = 20", {
  eav <- 20L
  expect_equal(
    cAttr(eav),
    structure(
      list( # test checked, 23.2.2026
        Critical = 19 / 400,
        Success = 361/400,
        Fail = 19 / 400,
        Botch = 1/400),
      class = "CSFB", check = "Attribute"
    ),
    info = paste("eav = ", eav)
  )
})


#
#
#
test_that("cAttr: Brute force gives same result as efficient algorithm", {
  eav <- sample(1L:20, 1L)
  expect_equal(
    cAttr(eav),
    cAttr_BF(eav),
    info = paste("eav = ", eav)
  )
})


test_that("cAttr: sum is always 1", {
  eav <- 1:20
  result <- cAttr(eav)

  # sum up all results
  sums <- result |> as.data.frame.CSFB() |> rowSums()

  expect_equal(
    sums,
    rep(1.0, 20)
  )
})


