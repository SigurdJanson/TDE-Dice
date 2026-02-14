#' rect1d20
#'
#' Create a probability distributions for a d20 truncated at and
#' rectified to `eav`.
#'
#' @param eav The effective attribute value of the check
#' (integer, scalar).
#' @returns An vector representing the number of events
#' for each face from 1 to 20. `rect1d20` returns the number
#' of occurrences.
#'
#' @details These functions do not validate their arguments.
#' They are only defined for `1 <= eav <= 20`.
#'
#' @export
rect1d20 <- function(eav)
  c(rep(0L, eav-1L), eav, rep(1L, 20L - eav)) |>
  setNames(1:20)

