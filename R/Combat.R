# TODO : implement botch level

#' Hit Point Limits
#'
#' Helpers to determine the lowest and the highest possible
#' hit points.
#' @returns `.minHP` is an integer >= 0. `.maxHP` is an integer
#' with `.maxHP > .minHP`.
#' @noRd
#' @name HitPointLimits
#' @keywords internal
.minHP <- function(Count, Mod) Count + Mod
#' @describeIn HitPointLimits maximum hit points
.maxHP <- function(Count, Dice, Mod) ((Count * Dice) + Mod) * 2L



#' cAttack
#' The CSFB probabilities for an attack, parry, or dodge rolls.
#' @param eav the effective attribute to roll against.
#' @param bl The botch level, typically only a 20 is a botch, but some
#' weapons or maneouvers change that.
#'
#' @returns A list with 4 vectors: `Critical`, `Success`, `Fail`, `Botch`
#' (S3 class `CSFB`).
#' @export
#'
#' @examples
#' cCombat(12)
cCombat <- function(eav, bl = 20L) {
  stopifnot(`'eav' must be >= 1` = all(eav > 0))

  eav <- pmin(eav, 20L)
  result <- newCSFB(
    cr = eav / 400.0,
    su = pmin(19*eav / 400.0, (19*20-1) / 400), # pmax corrects p(EAV=20)
    fa = (380 - 19*eav) / 400.0,
    bo = pmax(20 - eav, 1) / 400.0, # pmax corrects p(EAV=20)
    check = "Combat"
  )
  return(result)
}


# TODO: add 0 hp to dhitpoints
# TODO: handle weapons mod that lead to 0 hit points

#' Hit point distribution
#'
#' @param x,q vector of quantiles, the number of hit points
#' @param eav The effective attribute value of the check
#' (integer, scalar).
#' @param w the weapon specifier, a \code{\link[newRoll]{Roll}}
#' object with die count, faces, and modifier or a string that
#' can be coerced into it.
#' @param bl botch level (integer, [18, 20])
#'
#' @returns
#' `dhitpoints` gives the density, `phitpoints` gives the distribution
#' function, `qhitpoints` gives the quantile function and
#' `rhitpoints` generates random deviates.
#'
#' Throws en error when x is empty or outside
#'
#' @name HitPointDistribution
#' @aliases DiscreteHitPoints
#' @aliases dhitpoints
#'
#' @keywords distribution
#' @concept Univariate
#' @concept Discrete
#'
#' @export
#'
#' @examples
#' dhitpoints(6, 12, c(Count=2, Dice=6, Mod=0))
dhitpoints <- function(x, eav, w, bl = 20L) {
  if(length(x) == 0) return(numeric())
  stopifnot(`Only a single attack value is supported` = length(eav) == 1)

  minHP <- .minHP(w["Count"], w["Mod"])
  maxHP <- .maxHP(w["Count"], w["Dice"], w["Mod"])
  stopifnot(`Requested quantiles outside range of distribution` =
              all(1 <= x) && all(x <= maxHP))

  if (w["Count"] == 1)
    dhp <- rep(1/w["Dice"], w["Dice"])
  else if (w["Count"] == 2)
    dhp <- dSum2dN(w["Dice"]) # for efficiency reasons
  else
    dhp <- dSumKdN(w["Count"], w["Dice"])

  eav <- min(eav, 20L)
  Critical = eav / 400.0 # probability of a critical success
  Success = eav / 20 - Critical # probability of a regular success

  indices <- 1L:length(dhp)+minHP-1L
  result <- numeric(maxHP)
  result[indices] <- Success * dhp
  result[indices * 2] <- result[indices * 2] + Critical * dhp

  return(
    ifelse(result[x] > 0 & result[x] < 21, result[x], 0) |>
      as.numeric() |>
      setNames(x)
  )
}

#
#' @describeIn HitPointDistribution description
phitpoints <- function(q, eav, w, bl = 20L, lower.tail = TRUE) {
  maxHP <- .maxHP(w["Count"], w["Dice"], w["Mod"])
  if (lower.tail)
    select <- 1:max(q)
  else
    select <- 1:maxHP

  dhp <- dhitpoints(select, eav, w, bl)
  if (lower.tail)
    php <- cumsum(dhp) # cumsum preserves names
  else
    php <- rev(cumsum(rev(dhp)))
  return(php[q])
}


#' @describeIn HitPointDistribution description
#' @param p vector of probabilities.
qhitpoints <- function(p, eav, w, bl = 20L, lower.tail = TRUE) {
  stopifnot(`'eav' must be a scalar within 1-20` =
              length(eav) == 1L, eav >= 1 && eav <= 20)
  stopifnot(!is.null(p) && !is.na(p))
  if (any(p < 0 | p > 1)) stop("p must be between 0 and 1")

  if (!lower.tail) {
    p <- 1 - p
  }

}




#' @describeIn HitPointDistribution description
#' @param n number of observations. If `length(n) > 1`,
#' the length is taken to be the number required.
rhitpoints <- function(n, eav, w, bl = 20L) {
  stopifnot(`'eav' must be a scalar within 1-20` =
              length(eav) == 1L, eav >= 1 && eav <= 20)
  if (length(n) > 1L)
    n <- length(n)
  else if (length(n) == 0L || n == 0L)
    return(integer())
  else if (n < 0)
    stop("'n' must be greater than 0")

  minHP <- .minHP(w["Count"], w["Mod"])
  maxHP <- .maxHP(w["Count"], w["Dice"], w["Mod"])
  HP <- minHP:maxHP

  chances <- cCombat(eav, bl)
  p <- dhitpoints(HP, eav, w, bl)
  p <- c(chances$Fail + chances$Botch, p)
  #DEBUG: if (length(c(0, HP)) != length(p)) stop(length(c(0, HP)), "!=", length(p))
  sample(c(0, HP), n, replace=TRUE, prob = p)
}
