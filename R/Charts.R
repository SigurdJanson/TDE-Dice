library(ggplot2)



#' Creates a plot that shows the probabilities of attribute checks.
#'
#' @returns A ggplot object
#' @export
#'
#' @examples
#' plotAttributeChecks()
plotAttributeChecks <- function() {
  require(ggplot2)
  ylabels <- \(x) paste(x, "%")

  df <- cAttr(1:24) |>
    as.data.frame() |>
    cbind(EAV = 1L:24L) |>
    reshape(idvar = "EAV", varying = list(1:4), v.names = "p", timevar = "Outcome", direction = "long")
  df$Outcome <- as.factor(df$Outcome)
  df$p <- df$p * 100

  ggplot(df, aes(x = EAV, y = p, group=Outcome, colour=Outcome)) +
    geom_point() +
    geom_line() +
    guides(color = guide_legend(title = "Outcome")) +
    scale_color_manual(values = c("#00876c", "#78ab63", "#e18745", "#d43d51"),
                       labels = c("Critical", "Success", "Fail", "Botch")) +
    scale_x_continuous(breaks = seq(0, 25, by = 10)) +
    scale_y_continuous(breaks = seq(0, 100, by = 10),
                       labels = ylabels) +
    labs(
      x = "Effective Attribute Value",
      y = "Probability",
      title = "Probabilities of Attribute Check Outcomes"
    ) +
    theme_minimal()
}


#' Skill Check Plots
#'
#' Plot the probabilities of a skill check. Outcomes are plotted on
#' the x-axis with probabilities on y. Skill point remainders,
#'
#' @param eav A vector with three integers as effective attribute values.
#' @param skill Skill level (integer, scalar)
#'
#' @returns A ggplot object
#' @export
#'
#' @examples
#' plotSkillRemainder(c(7, 10, 13), 8)
plotSkillRemainder <- function(eav, skill) {
  require(ggplot2)
  .Palette <- c("#999999FF", # FAIL
                "#1B641B", "#269C29", "#33D23B", "#68E170", # QL1 - 4
                "#9EEEA6", "#D7F9DB", "#EDFFDB") # QL 5 - 7

  sp <- dSkillPoints(-1:skill, eav, skill, "df")
  ql <- qualityLevel(sp$Remainder)
  sp$QL <- ql |> .qlfactor()

  ylabels <- \(x) paste(x, "%")
  Title <- "Probabilities of Remaining Skill Points"
  SubTitle <- paste("EAV:", paste(eav, collapse="/"), paste("Skill:", skill))
  Palette <- .Palette[1L:(max(ql) + 1L)]

  ggplot(sp, aes(x = Remainder, y = p, fill = QL)) +
    geom_col() +
    scale_fill_manual(values = Palette) +
    scale_y_continuous(labels = ylabels) +
    labs(
      x = "Skill Remainder", y = "Probability",
      title = Title, subtitle = SubTitle) +
    theme_minimal()
}



#' @describeIn plotSkillRemainder Plots the skill check probabilities (y-axis)
#' as a function of aggregated die rolls (x-axis).
#' @param add3d20 if `TRUE` the distribution of the sum of 3d20
#' is added (logical, scalar)
#' @export
#' @examples
#' plotSkillAggregateDies(c(7, 10, 13), 8, TRUE)
plotSkillAggregateDies <- function(eav, skill, add3d20 = FALSE) {
  require(ggplot2)
  .Palette <- c("#999999FF", # FAIL
                "#1B641B", "#269C29", "#33D23B", "#68E170", # QL1 - 4
                "#9EEEA6", "#D7F9DB", "#EDFFDB") # QL 5 - 7
  # DATA
  x <- 1:60L
  ql <- TDEDice:::qualityLevel(skill:0)
  xLenFails <- length(x) - (sum(eav) - 1L) - (skill + 1L)
  df <- data.frame(
    p3d20 = d3D20(x),
    pMaxSum = dSkillPurged(1:60, eav, skill, "vector") +
      crit3d20(eav)/8000 + botch3d20(eav)/8000,
    DieRoll = x,
    QL = c(Below = rep(0L, sum(eav)-1L), ql, rep(0L, xLenFails)) |>
      TDEDice:::.qlfactor()
  )

  # CHART INFO
  Title <- "Probabilities of Remaining Skill Points"
  SubTitle <- paste("EAV:", paste(eav, collapse="/"), paste("Skill:", skill))
  Palette <- .Palette[1L:(max(ql) + 1L)]
  ylabels <- \(x) paste(x, "%")

  # PLOT
  pl <- ggplot(df, aes(x=DieRoll, y=pMaxSum, fill = QL)) +
    geom_col() +
    scale_fill_manual(values = Palette) +
    scale_y_continuous(labels = ylabels) +
    labs(
      x = "Aggregated Dies", y = "Probability",
      title = Title, subtitle = SubTitle,
      caption = "Outcomes of a aggregated 3d20 roll projected to skill remainder.") +
    theme_minimal()
  if (isTRUE(add3d20)) {
    pl <- pl +
      geom_step(data = df, aes(x=DieRoll, y=p3d20), group = 1L, colour = "red")
  }

  return(pl)
}
