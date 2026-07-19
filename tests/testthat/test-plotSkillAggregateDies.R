library(testthat)

testPlotStructure(
  plotSkillAggregateDies(rep(10L, 3L), 6L),
  expectedGeom = c("GeomCol"),
  expectedX = "DieRoll",
  expectedY = "pMaxSum"
)

testPlotAESDimensions(
  plotSkillAggregateDies(rep(10L, 3L), 6L),
  columns = c("x", "y", "colour"),
  nRow = 60
)

testPlotData(
  plotSkillAggregateDies(rep(10L, 3L), 6L),
  columns = c("p3d20", "pMaxSum", "DieRoll", "QL"),
  nRow = 60
)

testPlotLabels(
  plotSkillAggregateDies(rep(10L, 3L), 6L),
  x ="Aggregated Dies",
  y = "Probability",
  title = "Probabilities of Remaining Skill Points",
  subtitle = "EAV: 10/10/10 Skill: 6",
  caption = "Outcomes of a aggregated 3d20 roll projected to skill remainder."
)
testPlotLabels(
  plotSkillAggregateDies(rep(13L, 3L), 13L),
  x ="Aggregated Dies",
  y = "Probability",
  title = "Probabilities of Remaining Skill Points",
  subtitle = "EAV: 13/13/13 Skill: 13",
  caption = "Outcomes of a aggregated 3d20 roll projected to skill remainder."
)


test_that("printing actually works",{
  p <- plotSkillAggregateDies(rep(10L, 3L), 6L)
  expect_error(print(p), NA)
})


# needed for RStudio to recognize the test file
test_that("Intentionally left blank", {
  skip("")
})
