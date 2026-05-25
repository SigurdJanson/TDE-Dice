# CHARTS


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

  ggplot(df, aes(x = EAV, y = p, group=Outcome, color=Outcome)) +
    geom_point() +
    geom_line() +
    guides(color = guide_legend(title = "Outcome")) +
    scale_color_manual(values = c("#00876c", "#78ab63", "#e18745", "#d43d51"),
                       labels = c("Critical", "Success", "Fail", "Botch")) +
    scale_x_continuous(breaks = seq(0, 25, by = 10)) +
    scale_y_continuous(breaks = seq(0, 100, by = 10),
                       labels = ylabels) +
    xlab("Effective Attribute Value") +
    ylab("Probability") +
    theme_minimal()
}
