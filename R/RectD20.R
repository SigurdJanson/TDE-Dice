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


#' Rectified 1d20 distribution
#'
#' These functions provide information about the
#' distribution of a d20 which has been truncated and
#' rectified at the effective attribute value.
#' `dRect1d20` gives the density, `pRect1d20` gives the
#' distribution function, `qRect1d20` gives the quantile function,
#' and `rRect1d20` generates random deviates.
#'
#' @param x,q vector of quantiles, the sides of the die.
#' @param eav The effective attribute value of the check
#' (integer, scalar).
#'
#' @details
#' The distribution is a discrete uniform distribution truncated at
#' and rectified to `eav`. That means all events below ´eav` are
#' treated to `eav`. Another interpretation is a weighted distribution
#' with all weights below `eav` being 0 and the weight for `p(eav)`
#' is `eav`.
#'
#' This distribution is required to compute the distribution for
#' skill checks.
#'
#' `dRect1d20` gives the density, `pRect1d20` gives the
#' distribution function, `qRect1d20` gives the quantile function,
#' and `rRect1d20` generates random deviates.
#'
#' @seealso \link{dSkill}, \link[extraDistr]{ddunif}
#'
#' @name RectifiedUniform1d20
#' @aliases Discrete1d20Uniform
#' @aliases dRect1d20
#'
#' @keywords distribution
#' @concept Univariate
#' @concept Discrete
#'
#' @export
dRect1d20 <- function(x, eav) {
  stopifnot(length(eav) == 1L, eav >= 1 && eav <= 20,
            !is.null(x) && all(x >= 1) && all(x <= 20))
  return(rect1d20(eav)[x] / 20)
}


#' @param lower.tail logical; if `TRUE` (default), probabilities are
#' \eqn{X \le x}, otherwise, \eqn{X \ge x}.
#' @describeIn RectifiedUniform1d20 Distribution function for a truncated and
#' rectified d20 roll.#'
#' @export
pRect1d20 <- function(q, eav, lower.tail = TRUE) {
  stopifnot(`'eav' must be a scalar within 1-20` =
              length(eav) == 1L, eav >= 1 && eav <= 20)
  stopifnot(!is.null(q) && all(q >= 1) && all(q <= 20))

  p <- rect1d20(eav) / 20
  if (lower.tail) {
    p <- cumsum(p[1:max(q)])
    p <- p[q]
  }
  else {
    i <- min(q)
    p <- cumsum(p[20:i])
    p <- rev(p)[q-i+1L]
  }
  return(p)
}



#' @param p vector of probabilities.
#' @describeIn RectifiedUniform1d20 Quantile function for a truncated and
#' rectified d20 roll.
#' @export
qRect1d20 <- function(p, eav, lower.tail = TRUE) {
  stopifnot(`'eav' must be a scalar within 1-20` =
              length(eav) == 1L, eav >= 1 && eav <= 20)
  stopifnot(!is.null(p) && !anyNA(p))
  if (any(p < 0 | p > 1)) stop("p must be between 0 and 1")

  if (!lower.tail) {
    p <- 1 - p
  }

  # Discrete quantile: smallest k such that P(X <= k) >= p
  quantile <- (p * 20 - 1e-12) |> pmax(eav) |>
    ceiling() |> # no fractions
    pmin(20) # not greater than 1

  return(pmax(quantile, eav))
}



#' @param n number of observations. If `length(n) > 1`,
#' the length is taken to be the number required.
#' @describeIn RectifiedUniform1d20 Random generator function for a
#' truncated and rectified d20 roll.
#' @export
rRect1d20 <- function(n, eav) {
  stopifnot(`'eav' must be a scalar within 1-20` =
              length(eav) == 1L, eav >= 1 && eav <= 20)
  if (length(n) > 1L)
    n <- length(n)
  else if (length(n) == 0L || n == 0L)
    return(integer())
  else if (n < 0)
    stop("'n' must be greater than 0")

  roll <- sample(1:20, size = n, replace = TRUE)
  return(pmax(roll, eav))
}
