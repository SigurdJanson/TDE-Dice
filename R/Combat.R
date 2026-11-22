# TODO : implement botch level

#' HitPointLimits
#'
#' Helpers to determine the lowest and the highest possible
#' hit points.
#' @returns `.minHP` is an integer >= 0. `.maxHP` is an integer
#' with `.maxHP > .minHP`.
#' @name HitPointLimits
#' @noRd
#' @keywords internal
.minHP <- function(Count, Mod) Count + ifelse(is.na(Mod), 0, Mod)
#'HitPointLimits: Maximum hit points
#' @noRd
#' @keywords internal
.maxHP <- function(Count, Dice, Mod) ((Count * Dice) + ifelse(is.na(Mod), 0, Mod)) * 2L



#' cAttack
#' The CSFB probabilities for an attack, parry, or dodge rolls.
#' @param eav the effective attribute to roll against.
#' @param bl The botch level, typically only a 20 is a botch, but some
#' weapons or maneouvers change that.
#'
#' @returns A list with 4 vectors: `Critical`, `Success`, `Fail`, `Botch`
#' (S3 class `CSFB`).
#'
#' @seealso \link{cAttr}, \link{cSkill}, \link{CSFB}
#'
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
#' The return vector is named with hit point values.
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
#' dhitpoints(6, 12, c(Count=2, Faces=6, Mod=0))
dhitpoints <- function(x, eav, w, bl = 20L) {
  if(length(x) == 0) return(numeric())
  if (anyNA(x)) return(NA_real_)
  stopifnot(`Only a single combat skill 'eav' value is supported` = length(eav) == 1)

  # --- Calculate Attack Probabilities ---
  eav <- min(eav, 20L)
  p_crit <- eav / 400.0
  p_success <- eav / 20.0 - p_crit
  p_fail <- 1.0 - p_success - p_crit

  # Guard against floating point slight negatives
  p_success <- max(0.0, p_success)
  p_fail <- max(0.0, p_fail)

  # --- Get Dice Distribution ---
  if (w["Count"] == 1L) {
    dhp <- rep(1/w["Faces"], w["Faces"])
    dice_sums <- 1:w["Faces"]
  } else if (w["Count"] == 2L) {
    dhp <- dSum2dN(w["Faces"])
    dice_sums <- 2:(2 * w["Faces"])
  } else {
    dhp <- dSumFdN(w["Count"], w["Faces"])
    dice_sums <- w["Count"]:(w["Count"] * w["Faces"])
  }

  # --- Calculate HP Values ---
  # Apply modifier
  raw_hp <- dice_sums + w["Mod"]

  # Normal hit points (clamped to 0)
  hp_normal <- pmax(0L, raw_hp)

  # Critical hit points (double raw, clamped to 0)
  hp_critical <- pmax(0L, 2L * raw_hp)

  # Maximum possible HP in the distribution
  max_hp <- max(hp_critical)

  # --- Build Probability Mass Function ---
  # Vector index i corresponds to HP value (i-1)
  pmass <- numeric(max_hp + 1L)

  # 1. Add Failure mass to HP = 0
  pmass[1L] <- pmass[1L] + p_fail

  # 2. Add Success mass
  # Using tabulate for efficiency if dhp is large, but loop is clear and safe for small dice
  # idx_norm is 1-based index for hp_normal
  idx_norm <- hp_normal + 1L
  # Accumulate
  for(i in seq_along(dhp)) {
    pmass[idx_norm[i]] <- pmass[idx_norm[i]] + (p_success * dhp[i])
  }

  # 3. Add Critical mass
  idx_crit <- hp_critical + 1L
  for(i in seq_along(dhp)) {
    pmass[idx_crit[i]] <- pmass[idx_crit[i]] + (p_crit * dhp[i])
  }

  # --- Extract Results for x ---
  result <- numeric(length(x))
  names(result) <- x

  # Identify valid x values (within support [0, max_hp])
  valid <- (x >= 0) & (x <= max_hp)

  if(any(valid)) {
    # Map x to 1-based index
    indices <- x[valid] + 1L
    result[valid] <- pmass[indices]
  }

  # Values outside support remain 0
  return(
    result |>
      as.numeric() |>
      setNames(x)
  )
}



#' @rdname HitPointDistribution
#' @param lower.tail logical; if TRUE (default), probabilities are
#' \eqn{X \le x}, otherwise, \eqn{X \ge x}.
#' @export
phitpoints <- function(q, eav, w, bl = 20L, lower.tail = TRUE) {
  if(length(q) == 0) return(numeric())
  if(anyNA(q)) return(NA_real_)
  stopifnot(is.logical(lower.tail), length(lower.tail) == 1L, !is.na(lower.tail))

  maxHP <- .maxHP(w["Count"], w["Faces"], w["Mod"])

  if (lower.tail)
    select <- 0:max(q)
  else
    select <- 0:maxHP
  dhp <- dhitpoints(select, eav, w, bl)

  if (lower.tail)
    php <- cumsum(dhp) # cumsum preserves names
  else
    php <- rev(cumsum(rev(dhp)))

  # Use as.character(q) to correctly subset by names, avoiding positional
  # indexing issues (which would fail on 0 and shift the rest by 1)
  return(php[as.character(q)])
}


#' @rdname HitPointDistribution
#' @param p vector of probabilities.
#' @export
qhitpoints <- function(p, eav, w, bl = 20L, lower.tail = TRUE) {
  # stopifnot(`'eav' must be a scalar within 1-20` =
  #             length(eav) == 1L, eav >= 1 && eav <= 20)
  stopifnot(!is.null(p))
  stopifnot(is.logical(lower.tail), length(lower.tail) == 1L, !is.na(lower.tail))
  if (any(p < 0 | p > 1, na.rm = TRUE))
    stop("p must be between 0 and 1")

  # If lower.tail is FALSE, we are looking for P(X >= x) <= p,
  # which is equivalent to P(X < x) >= 1 - p.
  if (!lower.tail) {
    p <- 1 - p
  }

  maxHP <- .maxHP(w["Count"], w["Faces"], w["Mod"])
  support <- 0:maxHP

  # Compute the CDF for all possible values using the corrected phitpoints
  cdf <- phitpoints(support, eav, w, bl, lower.tail = TRUE)

  # For each probability p, find the smallest HP value (x) where CDF(x) >= p
  q_vals <- sapply(p, \(pi) {
    if (is.na(pi)) return(NA_integer_)

    idx <- which(cdf >= pi)[1]
    # Fallback to maxHP if floating point inaccuracies cause pi to slightly exceed the max CDF
    if (is.na(idx))
      maxHP
    else
      support[idx]
  })

  return(as.integer(q_vals))
}




#' @rdname HitPointDistribution
#' @param n number of observations. If `length(n) > 1`,
#' the length is taken to be the number required.
#' @export
rhitpoints <- function(n, eav, w, bl = 20L) {
  # check and prepare arguments
  stopifnot(`'eav' must be a scalar >= 1` =
              length(eav) == 1L, eav >= 1)
  eav <- min(eav, maxd20)

  if (length(n) > 1L)
    n <- length(n)
  else if (length(n) == 0L || n == 0L)
    return(integer())
  else if (n < 0)
    stop("'n' must be greater than 0")

  maxHP <- .maxHP(w["Count"], w["Faces"], w["Mod"])
  support <- 0:maxHP
  p <- dhitpoints(support, eav, w, bl)
  sample(support, n, replace=TRUE, prob = p)
}
