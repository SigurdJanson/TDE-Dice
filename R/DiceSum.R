
#' @title dSumKdN
#' Calculate the probability of rolling n dice with k faces.
#' @param k The number of dice (integer, `k > 1`)
#' @param n The number of faces of the dice (integer, `n > 1`)
#'
#' @details For two fair n-sided dice (faces 1..n),
#' the sum distribution is triangular. The functions here have
#' been tested in a value range of n ∊ [1, 100].
#' @returns A named vector with the probabilities for each sum
#' of dice.
#' @export
#'
#' @examples
#' dSumKdN() # 2d6
#' dSumKdN(4, 6) # 4d6
#' dSumKdN(3, 20) # 3d20
dSumKdN <- function(k = 2L, n = 6L) {
  stopifnot(is.numeric(n), n %% 1 == 0, n >= 1L)
  stopifnot(is.numeric(k), k %% 1 == 0, k >= 1L)

  min_sum <- k
  max_sum <- k * n
  sums <- min_sum:max_sum
  total_outcomes <- n^k  # May overflow for very large n/k (use logs if needed)

  # Precompute binomial coefficients for efficiency
  probs <- vapply(sums, \(s) {
    T_val <- s - k  # Total after shifting to 0..n-1 range
    max_j <- T_val %/% n  # floor((s-k)/n)

    if (max_j < 0) return(0)  # Impossible sum

    j_vals <- 0:max_j
    signs <- ifelse(j_vals %% 2L == 0L, 1, -1)

    # Compute terms: (-1)^j * C(k,j) * C(s - n*j - 1, k-1)
    terms <- signs *
      choose(k, j_vals) *
      choose(s - n * j_vals - 1L, k - 1L)

    sum(terms, na.rm = TRUE) / total_outcomes
  }, numeric(1))

  # Handle floating-point errors (probabilities should sum to 1)
  probs <- pmax(0, probs)  # Clip negatives from rounding
  probs <- probs / sum(probs)  # Renormalize

  return(setNames(probs, sums))
}



#' dSum2dN
#' @details For two fair n-sided dice (faces 1..n),
#' the sum distribution is triangular.
#' @describeIn sumKdN `sum2dN` is used for 2 dice because this
#' method is a low more efficient.
dSum2dN <- function(n = 6L) {
  stopifnot(n %% 1 == 0)
  stopifnot(n > 1)

  maxSum <- 2L*n
  sums <- 2L:maxSum  # Possible sums
  # Peak at n+1; symmetric rise/fall
  p <- pmin(sums - 1L, maxSum + 1 - sums) / n^2
  setNames(p, sums)
}
