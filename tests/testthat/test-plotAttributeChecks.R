library(testthat)

testPlotStructure(
  plotAttributeChecks(),
  expectedGeom = c("GeomPoint", "GeomLine"),
  expectedX = "EAV",
  expectedY = "p"
)

testPlotAESDimensions(
  plotAttributeChecks(),
  columns = c("x", "y", "group", "colour"),
  nRow = 4L * 24L
)

testPlotData(
  plotAttributeChecks(),
  columns = c("Outcome", "EAV", "p"),
  nRow = 4L * 24L
)

testPlotLabels(
  plotAttributeChecks(),
  x ="Effective Attribute Value",
  y = "Probability",
  title = "Probabilities of Attribute Check Outcomes"
)

test_that("printing actually works",{
  p <- plotAttributeChecks()
  expect_error(print(p), NA)
})


# needed for RStudio to recognize the test file
test_that("Intentionally left blank", {
  skip("")
})
