library(testthat)

testPlotStructure(
  plotSkillRemainder(rep(10L, 3L), 6L),
  expectedGeom = c("GeomCol"),
  expectedX = "Remainder",
  expectedY = "p"
)

testPlotAESDimensions(
  plotSkillRemainder(rep(10L, 3L), 6L),
  columns = c("x", "y", "colour"),
  nRow = 6 + 1 + 1
)

testPlotData(
  plotSkillRemainder(rep(10L, 3L), 6L),
  columns = c("Remainder", "p", "QL"),
  nRow = 6 + 1 + 1
)

testPlotLabels(
  plotSkillRemainder(rep(10L, 3L), 6L),
  x ="Skill Remainder",
  y = "Probability",
  title = "Probabilities of Remaining Skill Points",
  subtitle = "EAV: 10/10/10 Skill: 6"
)
testPlotLabels(
  plotSkillRemainder(rep(13L, 3L), 13L),
  x ="Skill Remainder",
  y = "Probability",
  title = "Probabilities of Remaining Skill Points",
  subtitle = "EAV: 13/13/13 Skill: 13"
)


test_that("printing actually works",{
  p <- plotSkillRemainder(rep(10L, 3L), 6L)
  expect_error(print(p), NA)
})


# needed for RStudio to recognize the test file
test_that("Intentionally left blank", {
  skip("")
})
