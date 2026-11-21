
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


