library(testthat)

testPlotStructure(
  plotSkillQL(rep(10L, 3L), 6L),
  expectedGeom = c("GeomCol"),
  expectedX = "QL",
  expectedY = "p"
)

testPlotAESDimensions(
  plotSkillQL(rep(10L, 3L), 6L),
  columns = c("x", "y", "colour"),
  nRow = length(0:.maxql)
)

testPlotData(
  plotSkillQL(rep(10L, 3L), 6L),
  columns = c("p", "QL"),
  nRow = length(0:.maxql)
)

testPlotLabels(
  plotSkillQL(rep(10L, 3L), 6L),
  x = "Quality Level",
  y = "Probability",
  title = "Probabilities of Remaining Skill Points",
  subtitle = "EAV: 10/10/10 Skill: 6"
)
testPlotLabels(
  plotSkillQL(rep(13L, 3L), 13L),
  x = "Quality Level",
  y = "Probability",
  title = "Probabilities of Remaining Skill Points",
  subtitle = "EAV: 13/13/13 Skill: 13"
)


test_that("printing actually works",{
  p <- plotSkillQL(rep(10L, 3L), 6L)
  expect_error(print(p), NA)
})


# needed for RStudio to recognize the test file
test_that("Intentionally left blank", {
  skip("")
})
