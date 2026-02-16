test_that("cAttr_BF: eav = 10", {
  eav <- 10L
  expect_equal(
    cAttr_BF(eav),
    structure(
      list(
        Critical = 0.5 / 20,
        Success = 9.0 / 20 + 0.5 / 20,
        Fail = 9.0 / 20 + 0.5 / 20,
        Botch = 0.5 / 20),
      class = "CSFB", check = "Attribute"
    ),
    info = paste("eav = ", eav)
  )
})

test_that("cAttr: eav = 10", {
  eav <- 10L
  expect_equal(
    cAttr(eav),
    structure(
      list(
        Critical = 0.5 / 20,
        Success = 9.0 / 20 + 0.5 / 20,
        Fail = 9.0 / 20 + 0.5 / 20,
        Botch = 0.5 / 20),
      class = "CSFB", check = "Attribute"
    ),
    info = paste("eav = ", eav)
  )
})


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
  sums <- result |> as.data.frame() |> rowSums()

  expect_equal(
    sums,
    rep(1.0, 20)
  )
})


