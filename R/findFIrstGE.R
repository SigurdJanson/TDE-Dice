#' findFirstGE
#'
#' Find the first index `i` in `x` for which `x[i] >= target` holds true.
#' @param x A numeric vector with monotonously increasing values.
#' @param target A numeric vector.
#' @param forward specifies the search direction. `TRUE` (the default) searches
#' from left-to-right(low indices to high ones), `FALSE` searches in the other
#' direction.
#' @details This is a helper function for quantile functions.
#' It may also work outside the value range [0, 1], but has been tested
#' most extensively within this range.
#' \itemize{
#' \item{Leading zeroes}{Leading zeroes are ignored.}
#' \item{Precision}{The comparison uses a precision of 1E-8. Values with differences
#' smaller than that will be treated as equal.}
#' }
#'
#' @returns For each target it returns the index of the first `x[i]`
#' for which `x[i] > target` holds true; if several `x[i]` are equal
#' it moves the value to the foot of the next slope.
#'
#' If `target` is greater than any `x`, it returns `NA`.
findFirstGE <- function(x, target, forward = TRUE) {
  if (!is.numeric(x)) stop("'x' must be a numeric vector")
  if (!is.numeric(target)) stop("'target' must be a numeric vector")
  if (!is.logical(forward)) stop("'forward' must be a logical vector")
  if (length(forward) > 1L) forward <- forward[1L]
  .Call('_TDEDice_findFirstGE', PACKAGE = 'TDEDice', x, target, forward)
}
