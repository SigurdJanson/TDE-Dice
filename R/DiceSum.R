
#' dSumFdN
#'
#' Calculate the probability of rolling `n` dice with `f` faces.
#'
#' @param f number of dice (integer, `f > 1`)
#' @param n number of faces of the dice (integer, `n > 1`)
#'
#' @details For two fair n-sided dice (faces 1..n),
#' the sum distribution is triangular. The functions here have
#' been tested in a value range of n ∊ [1, 100].
#' @returns A named vector with the probabilities for each sum
#' of dice.
#'
#' @keywords distribution
#' @concept Univariate
#' @concept Discrete
#' @export
#'
#' @examples
#' dSumFdN() # 2d6
#' dSumFdN(4, 6) # 4d6
#' dSumFdN(3, 20) # 3d20
dSumFdN <- function(f = 2L, n = 6L) {
  stopifnot(is.numeric(n), n %% 1 == 0, n >= 1L)
  stopifnot(is.numeric(f), f %% 1 == 0, f >= 1L)

  min_sum <- f
  max_sum <- f * n
  sums <- min_sum:max_sum
  total_outcomes <- n^f  # May overflow for very large n/f (use logs if needed)

  # Precompute binomial coefficients for efficiency
  probs <- vapply(sums, \(s) {
    T_val <- s - f  # Total after shifting to 0..n-1 range
    max_j <- T_val %/% n  # floor((s-f)/n)

    if (max_j < 0) return(0)  # Impossible sum

    j_vals <- 0:max_j
    signs <- ifelse(j_vals %% 2L == 0L, 1, -1)

    # Compute terms: (-1)^j * C(f,j) * C(s - n*j - 1, f-1)
    terms <- signs *
      choose(f, j_vals) *
      choose(s - n * j_vals - 1L, f - 1L)

    sum(terms, na.rm = TRUE) / total_outcomes
  }, numeric(1))

  # Handle floating-point errors (probabilities should sum to 1)
  probs <- pmax(0, probs)  # Clip negatives from rounding
  probs <- probs / sum(probs)  # Renormalize

  return(setNames(probs, sums))
}



#' @describeIn dSumKdN `sum2dN` is used for 2 dice because this
#' method is a low more efficient.
#' @details For two fair n-sided dice (faces 1..n),
#' the sum distribution is triangular.
dSum2dN <- function(n = 6L) {
  stopifnot(n %% 1 == 0)
  stopifnot(n > 1)

  maxSum <- 2L*n
  sums <- 2L:maxSum  # Possible sums
  # Peak at n+1; symmetric rise/fall
  p <- pmin(sums - 1L, maxSum + 1 - sums) / n^2
  setNames(p, sums)
}
