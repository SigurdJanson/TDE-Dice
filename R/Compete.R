
#' Summarize the Head-to-Head Competition Between Two Independent
#' Discrete Random Variables
#'
#' @description
#' Given two independent discrete random variables
#' \eqn{X \sim \text{Dist}(\theta_A)}{X ~ Dist(theta_A)} and
#' \eqn{Y \sim \text{Dist}(\theta_B)}{Y ~ Dist(theta_B)},
#' this function computes the three probabilities that fully describe
#' their head-to-head outcome:
#'
#' \itemize{
#'   \item \code{A_wins}: \eqn{P(X > Y)}{P(X > Y)}
#'   \item \code{Tie}:    \eqn{P(X = Y)}{P(X = Y)}
#'   \item \code{P_wins}: \eqn{P(X < Y)}{P(X < Y)}
#' }
#'
#' These three values always sum to 1 (up to floating-point precision).
#' They are obtained by aggregating the full difference distribution
#' computed by \code{\link{dCompete}}.
#'
#' @section Note: this function does not check if `dfun` returns
#' valid results.
#'
#' @param dfun A density function for a discrete distribution
#'   (e.g., \code{\link{dql}}, \code{\link[stats]{dbinom}},
#'   \code{\link[stats]{dpois}}).
#' @param x A numeric vector of integer values defining the
#'   supported range over which the PMFs are evaluated.
#' @param aArgs A named list of parameters for distribution A of the active
#' opponent.
#' @param pArgs A named list of parameters for distribution P of the passive
#' opponent.
#' @param ties Are ties supported (`TRUE`) or counted as win for the passive
#' opponent (`FALSE`).
#'
#' @return A named numeric vector of length three with elements
#'   \code{A_wins}, \code{Tie}, and \code{P_wins}. Returns a vector of
#'   three \code{NA} values if invalid parameters are supplied.
#'
#' @seealso
#' \code{\link{Compete}} for the full difference distribution,
#' \code{\link{dCompete}} to query specific difference values.
#'
#' @family compete
#'
#' @examples
#' # Symmetric case: identical distributions
#' cCompete(
#'   dfun = dbinom,
#'   x  = 0:10,
#'   aArgs = list(size = 10, prob = 0.5),
#'   pArgs = list(size = 10, prob = 0.5)
#' )
#'
#' # A is clearly stronger
#' cCompete(
#'   dfun = dbinom,
#'   x  = 0:10,
#'   aArgs = list(size = 10, prob = 0.8),
#'   pArgs = list(size = 10, prob = 0.3)
#' )
#'
#' # Poisson example
#' cCompete(
#'   dfun     = dpois,
#'   x  = 0:30,
#'   aArgs = list(lambda = 5),
#'   pArgs = list(lambda = 3)
#' )
#'
#' @export
cCompete <- function(dfun, x, aArgs = NULL, pArgs = NULL, ties = TRUE) {
  pA <- do.call(dfun, c(list(x), aArgs))
  pP <- do.call(dfun, c(list(x), pArgs))

  joint <- outer(pA, pP)
  i <- row(joint) - 1L
  j <- col(joint) - 1L

  if (ties) {
    return(
      c(aWins = sum(joint[i >  j]),
        Tie   = sum(joint[i == j]),
        pWins = sum(joint[i <  j]))
    )
  } else {
    return(
      c(aWins = sum(joint[i >  j]),
        Tie   = 0.0,
        pWins = sum(joint[i <  j]) + sum(joint[i == j]))
    )
  }
}



##
## COMPETITION CHECK: DIFFERENCE SCORE ####
##



#' Compute the Probability Distribution of the Difference Between Two
#' Independent Discrete Random Variables
#'
#' @description
#' Given two independent discrete random variables
#' \eqn{X \sim \text{Dist}(\theta_A)}{X ~ Dist(theta_A)} and
#' \eqn{Y \sim \text{Dist}(\theta_B)}{Y ~ Dist(theta_B)},
#' this function computes the full probability mass function of their
#' difference \eqn{D = X - Y}{D = X - Y}.
#'
#' Mathematically, for each possible difference \eqn{d}:
#' \deqn{P(D = d) = \sum_{x} P(X = x) \cdot P(Y = x - d)}{
#'   P(D = d) = sum_x P(X = x) * P(Y = x - d)}
#'
#' This is equivalent to the discrete convolution of the PMF of \eqn{X}
#' with the reversed PMF of \eqn{Y}.
#' When `ties = FALSE`, probability mass at a difference of zero is moved
#' to a difference of negative one, counting ties as wins for the passive
#' opponent.
#'
#' @param dfun A density function for a discrete distribution
#'   (e.g., \code{\link{dql}}, \code{\link[stats]{dbinom}}, \code{\link[stats]{dpois}},
#'   \code{\link[stats]{dnbinom}}). It must accept a vector of integer
#'   values as its first argument and return a numeric vector of
#'   probabilities of the same length.
#' @param x A numeric vector of integer values defining the supported range
#'   over which the PMFs are evaluated. For unbounded distributions
#'   (e.g., Poisson), this should be a sufficiently wide truncation
#'   (e.g., \code{0:50}) such that the omitted tail probability is
#'   negligible.
#' @param aArgs A named list of parameters passed to \code{dfun} for
#'   distribution A (e.g., \code{list(size = 10, prob = 0.5)}).
#' @param pArgs A named list of parameters passed to \code{dfun} for
#'   distribution B.
#' @inheritParams cCompete ties
#'
#' @return A named numeric vector of probabilities, where each element
#'   corresponds to a possible value of \eqn{D = X - Y}. The names are
#'   the integer difference values (as character strings), and the vector
#'   is sorted in ascending order of these values. Returns \code{NA} if
#'   invalid parameters are supplied.
#'
#' @seealso
#' \code{\link{cCompete}} to summarize the three-way outcome
#' (A wins / Tie / P wins).
#'
#' @family compete
#'
#' @examples
#' # Binomial(10, 0.5) vs Binomial(10, 0.5)
#' dCompete(
#'   dfun = dbinom,
#'   x  = 0:10,
#'   aArgs = list(size = 10, prob = 0.5),
#'   pArgs = list(size = 10, prob = 0.5)
#' )
#'
#' # Poisson(5) vs Poisson(3)
#' dCompete(
#'   dfun = dpois,
#'   x  = 0:30,
#'   aArgs = list(lambda = 5),
#'   pArgs = list(lambda = 3)
#' )
#'
#' @export
dCompete <- function(dfun, x, aArgs = NULL, pArgs = NULL, ties = TRUE) {
  # Compute PMFs for both distributions
  pA <- do.call(dfun, c(list(x), aArgs))
  pB <- do.call(dfun, c(list(x), pArgs))

  # Validate inputs (return NA for invalid parameters)
  if (any(is.na(pA)) || any(is.na(pB)) ||
      any(pA < 0) || any(pB < 0)) {
    return(NA_real_)
  }

  # Compute all pairwise differences: support_A - support_P
  diffs <- outer(x, x, "-")
  if (!ties)
    diffs[diffs == 0] <- -1

  # Joint probabilities: P(X = a, Y = p) = P(X = a) * P(Y = p)
  probs <- outer(pA, pB)

  # Aggregate: sum probabilities for each unique difference value
  diff_table <- tapply(as.vector(probs), as.vector(diffs), sum)
  if (!ties) {
    if (!("-1" %in% names(diff_table)))
      diff_table <- c(diff_table, "-1" = 0)
    diff_table <- c(diff_table, "0" = 0)
  }

  # Return as named numeric vector, sorted by difference value
  result <- as.numeric(diff_table)
  names(result) <- as.numeric(names(diff_table))
  result[order(as.numeric(names(result)))]
}




#' Distribution Function for the Difference Between Two Independent Discrete
#' Random Variables
#'
#' @description
#' Computes the cumulative probability distribution of
#' \eqn{D = X - Y}, where \eqn{X} and \eqn{Y} are independent discrete
#' random variables. The distribution is obtained with \code{\link{dCompete}}.
#'
#' @param q A numeric vector of quantiles.
#' @inheritParams dCompete
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are
#'   \eqn{P(D \le q)}. If \code{FALSE}, probabilities are \eqn{P(D > q)}.
#'
#' @return A numeric vector of cumulative probabilities.
#'
#' @seealso
#' \code{\link{dCompete}} for the difference probability mass function,
#' \code{\link{qCompete}} for the quantile function.
#'
#' @family compete
#'
#' @examples
#' dCoins <- \(x) ifelse(x == 1, 0.5, ifelse(x == 2, 0.5, 0))
#'
#' pCompete(c(-1, 0, 1), dCoins, 0:3)
#'
#' @export
pCompete <- function(q, dfun, x, aArgs = NULL, pArgs = NULL, ties = TRUE,
                     lower.tail = TRUE) {
  stopifnot(!is.null(q), is.numeric(q), !anyNA(q))
  stopifnot(is.logical(lower.tail), length(lower.tail) == 1L, !is.na(lower.tail))

  probs <- dCompete(dfun, x, aArgs, pArgs, ties)
  if (length(probs) == 1L && is.na(probs))
    return(rep(NA_real_, length(q)))

  differences <- as.numeric(names(probs))
  cdf <- cumsum(probs)

  vapply(q, function(value) {
    if (value < differences[1L])
      lower_probability <- 0
    else if (value >= differences[length(differences)])
      lower_probability <- 1
    else
      lower_probability <- cdf[match(TRUE, differences > value) - 1L]

    if (lower.tail)
      lower_probability
    else
      1 - lower_probability
  }, numeric(1L))
}




