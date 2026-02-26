
# Skill related "constants"
maxd20 <- 20L # max faces of a d20
max3d20 <- 60L # the maximum sum of 3d20
totalEvents <- 8000L # total number of events with 3d20

.pSkillCriticals <- 58/8000
.pSkillBotches   <- 58/8000

#' d3D20
#'
#' The density function for rolling a sum of 3d20.
#'
#' @param x A vector of dice sums.
#'
#' @returns The vector of length 60 starts with index 1,
#' Index 1 and 2 are both zero because 3d20 have a sum of
#' at least 3.
#' @export
d3D20 <- function(x) {
  freq <- c(0, 0, 1, 3, 6, 10, 15, 21, 28,
            36, 45, 55, 66, 78, 91, 105, 120, 136, 153,
            171, 190, 210, 228, 244, 258, 270, 280, 288, 294,
            298, 300, 300, 298, 294, 288, 280, 270, 258, 244,
            228, 210, 190, 171, 153, 136, 120, 105, 91, 78,
            66, 55, 45, 36, 28, 21, 15, 10, 6, 3, 1)

  if (missing(x)) return(freq / 8000)

  x[x < 3L | x > 60L] <- 1L # handle indices of p that are out of range
  return(freq[x] / 8000)
}

# @describeIn d3D20 Distribution of critical successes
#' @describeIn d3D20 Distribution of critical successes
#' in a 3d20 which are defined by at least two instances
#' of a 1 in that roll.
#' @param eav effective attribute value
#' @export
crit3d20 <- function(eav) {
  partial <- function(aoi, alla) { # attribute of interest vs. all
    r <- c(rep(0, sum(alla)-1L), aoi, rep(1, 20-aoi), rep(0, 40))
    return(r[1:60])
  }

  result <- numeric(60L)
  for (a in eav) {
    result = result + partial(a, eav)
  }
  result[sum(eav)] <- result[sum(eav)] -2L

  return(result)
}

# @describeIn d3D20 Distribution of botches
#' @describeIn d3D20 Distribution of botches
#' in a 3d20 which are defined by at least two instances
#' of a 1 in that roll.
#' @param eav effective attribute value
#' @export
botch3d20 <- function(eav) {
  partial <- function(aoi) { # attribute of interest vs. all
    r <- c(rep(0, 39+aoi), aoi, rep(1, 20-aoi))
    return(r[1:60])
  }

  result <- numeric(60L)
  for (a in eav) {
    result = result + partial(a)
  }
  result[60L] <- result[60L] -2L

  return(result)
}




#' cSkill
#'
#'
#'
#' @param eav A vector with 3 places, each being an effective
#' attribute value.
#' @param skill The skill value
#'
#' @returns A CSFB object with 4 vectors:
#' `Critical`, `Success`, `Fail`, `Botch`.
#'
#' @seealso \link{cAttr}, \link{cCombat}, \link{CSFB}
#'
#' @export
#'
#' @examples
#' cSkill(c(11, 12, 14), 7)
cSkill <- function(eav, skill) {
  stopifnot(`'eav' must be greater than 0 to use a skill` =
              all(eav > 0))

  eav <- pmin(eav, maxd20)

  threshold <- min(sum(eav) + skill, max3d20) # separates successes from fails
  distr <- dSkillPurged(1:60, eav, skill)

  return(newCSFB(
    cr = .pSkillCriticals,
    su = sum(distr[1L:threshold]),
    fa = if (threshold < max3d20)
      sum(distr[(threshold+1L):max3d20])
    else
      0.0,
    bo = .pSkillBotches,
    check = "Skill")
  )
}





#' The likelihood for the outcomes of a skill check.
#' The likelihood for the outcomes of a skill check **excluding** critical
#' successes and botches.
#'
#' @param x A vector of dice sums.
#' @param eav A vector with 3 places, each being an effective
#' attribute value.
#' @param skill The skill value
#' @param format Determines how the output is generated, one of
#'
#'
#' @returns The result depends on the `format` argument.
#' \describe{
#'   \item{vector}{This is a vector of probabilities. The names of the
#'   vector elements denote the sum of dice.}
#'   \item{df}{A data frame with the columns:
#'   Outcome (each sum of dice), `p` (probability for this outcome),
#'   `Remainder` (remaining skill points), `QL` (the quality level)}
#' }
#' Each vector lists the probabilities for the given `x`.
#'
#' @export
dSkillPurged <- function(x, eav, skill, format = c("vector", "df")) {
  stopifnot(length(eav) == 3L)
  stopifnot(all(eav > 0L))
  format <- match.arg(format)

  #
  eav <- pmin(eav, maxd20)

  # Create the rectified 1d20 distributions and convolute them
  # into one distributions
  distr <- convolveDice(rect1d20(eav[1]), rect1d20(eav[2]))
  distr <- convolveDice(distr, rect1d20(eav[3]))
  distr <- distr / totalEvents
  # Remove criticals and botches from the distr.
  distr <- distr - (crit3d20(eav) + botch3d20(eav)) / totalEvents

  # FORMAT OUTPUT
  if (format == "df") {
    if (skill > 0)
      skillRemainder <- (skill-1):0
    else
      skillRemainder <- integer()
    skillRemainder <- c(rep(skill, sum(eav)), skillRemainder)[1:max3d20]
    skillRemainder <- c(rep(skill, sum(eav)), skillRemainder)
    if (length(skillRemainder) < max3d20)
      skillRemainder <- c(skillRemainder, rep(0L, max3d20-length(skillRemainder)))
      skillRemainder <- c(skillRemainder, rep(-1L, max3d20-length(skillRemainder)))
    else
      skillRemainder <- skillRemainder[1:max3d20]

    data <- data.frame(
      p = distr,
      Outcome = 1L:max3d20,
      Remainder = factor(skillRemainder),
      QL = factor(qualityLevel(skillRemainder),
                  levels = c(1:qualityLevel(skill), "0"),
                  labels = c(paste0("QL", 1:qualityLevel(skill)), "Failed"))
      Remainder = factor(skillRemainder, ordered = TRUE),
      QL = .qlfactor(qualityLevel(skillRemainder))
    )

    return(data)
  } else {
    return(setNames(distr, 1L:max3d20))
  }
}





#' QualityLevels
#'
#' Density, distribution function, quantile function and random
#' generation for the probability distribution
#' of quality levels of a skill check defined by effective attributes
#' (`eav`) and  a `skill` rating.
#'
#' @details "When making skill checks, the player has a pool of
#' skill points (SP), equal to the skill rating (SR) of the skill
#' in question, which can be spent to adjust failed die
#' rolls into successes. Leftover SP determine the QL" (Ulisses).
#' A failed skill check is identified by quality level of 0.
#'
#' @inheritParams dSkillPurged
#'
#' @param x,q vector of quantiles, the number of skill points
#' (integer, 0 <= x <= `r .maxql`).
#' @param p	vector of probabilities.
#' @param n	number of observations. If `length(n) > 1`, the length
#' is taken to be the number required.
#' @param lower.tail logical; if TRUE (default), probabilities are
#' \eqn{X \le x}, otherwise, \eqn{X \ge x}.
#'
#' @returns The result depends on the `format` argument.
#' \describe{
#'   \item{vector}{This is a vector of probabilities. The names of the
#'   vector elements denote the quality levels.}
#'   \item{df}{A data frame with the columns:
#'   Outcome (each sum of dice), `p` (probability for this outcome),
#'   `Remainder` (remaining skill points), `QL` (the quality level)}
#' }
#'
#' @references ('The Dark Eye' Game reference)[https://tde.ulisses-regelwiki.de/checks.html]
#' @export
#' @name QualityLevels
#'
#' @examples
#' dql(0:6, c(9, 10, 11), 8)
dql <- function(x, eav, skill, format = c("vector", "df")) {
  if (any(x < 0L | x > .maxql))
    stop("Quality levels are only defined for 0-6")
  # Arguments will be validated by `dSkillPurged()`
  format <- match.arg(format)

  data <- dSkillPurged(NA, eav, skill, "df")
  ql <- aggregate(p ~ QL, data, sum)
  # Add botches to 0 remaining skill points
  maxqlfound <- max(ql$QL)
  ql[ql$QL == min(ql$QL), "p"] <- ql[ql$QL == min(ql$QL), "p"] + .pSkillBotches
  ql[ql$QL == maxqlfound, "p"] <- ql[ql$QL == maxqlfound, "p"] + .pSkillCriticals


  # 1. Create a reference data frame with all 7 levels
  allLevels <- data.frame(QL=.qlfactor(0:.maxql))
  # 2. Merge with your existing data (df)
  # This keeps all levels from allLevels even if missing in df
  result <- merge(allLevels, ql[, c("p", "QL")], by = "QL", all.x = TRUE)
  result$p[is.na(result$p)] <- 0 # replace NAs in 'p' with 0
  result <- result[order(result$QL), ] # sort

  if (format == "df")
    return(result[x+1L,])
  else
    return(setNames(result$p, result$QL)[x+1L])
}


#' @rdname QualityLevels
#' @export
rql <- function(n, eav, skill) {
  stopifnot(`'eav' must be a scalar within 1-20` =
              length(eav) == 3L, all(eav >= 1) && all(eav <= 20))

  if (length(n) > 1L)
    n <- length(n)
  else if (length(n) == 0L || n == 0L)
    return(integer())
  else if (n < 0)
    stop("'n' must be greater than 0")

  p <- dql(0:.maxql, eav, skill)
  sample(0:.maxql, n, replace=TRUE, prob = p)
}


