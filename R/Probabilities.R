
# Skill related "constants"
maxd20 <- 20L # max faces of a d20
max3d20 <- 60L # the maximum sum of 3d20
totalEvents <- 8000L # total number of events with 3d20
.pSkillCriticals <- 58/8000
.pSkillBotches   <- 58/8000
.maxql <- 6L # max quality level

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





#' dSkillPurged
#'
#' The likelihood for the outcomes of a skill check **excluding** critical
#' successes and botches.
#'
#' @param x A vector of dice sums.
#' @param eav A vector with 3 places, each being an effective
#' attribute value.
#' @param skill The skill level
#' @param format Determines how the output is generated, one of
#'
#'
#' @returns The result depends on the `format` argument.
#' \describe{
#'   \item{vector}{This is a vector of probabilities. The names of the
#'   vector elements denote the sum of dice.}
#'   \item{df}{A data frame with the columns:
#'   Outcome (each sum of dice), `p` (probability for this outcome),
#'   `Remainder` (remaining skill points, with all failed checks being
#'   projected to a skill point remainder of `-1`.), `QL` (the quality level)}
#' }
#' Each vector lists the probabilities for the given sum of 3d20 `x`. </br>
#'
#'
#' @export
dSkillPurged <- function(x, eav, skill, format = c("vector", "df")) {
  stopifnot(length(eav) == 3L)
  stopifnot(all(eav > 0L))
  stopifnot(skill >= 0)
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
    skillRemainder <- c(rep(skill, sum(eav)), skillRemainder)
    if (length(skillRemainder) < max3d20)
      skillRemainder <- c(skillRemainder, rep(-1L, max3d20-length(skillRemainder)))
    else
      skillRemainder <- skillRemainder[1:max3d20]

    data <- data.frame(
      p = distr,
      Outcome = 1L:max3d20,
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
  ql <- aggregate(p ~ QL, data, sum, drop = FALSE)
  # max and min QL
  maxqlfound <- max(ql$QL[!is.na(ql$p)])
  qlZero <- levels(ql$QL)[1]
  # Fix the data frame: aggregate `drop=F` fills missing levels with NA
  ql$p[is.na(ql$p)] <- 0.0
  # Add botches to 0 remaining skill points
  ql[ql$QL == qlZero, "p"] <- ql[ql$QL == qlZero, "p"] + .pSkillBotches
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
pql <- function(q, eav, skill, lower.tail = TRUE, format = c("vector", "df")) {
  format <- match.arg(format)
  if (lower.tail)
    select <- 0:max(q)
  else
    select <- 0:.maxql

  dql <- dql(select, eav, skill, format = format)
  if (format == "vector") {
    if (lower.tail)
      result <- cumsum(dql) # cumsum preserves names
    else
      result <- rev(cumsum(rev(dql)))
    result <- result[q+1L]
  } else if (format == "df") {
    result <- dql
    if (lower.tail)
      result$p <- cumsum(dql$p) # cumsum preserves names
    else
      result$p <- rev(cumsum(rev(dql$p)))
    result <- result[q+1L,]
  }
  return(result)
}


#' @rdname QualityLevels
#' @export
qql <- function(p, eav, skill, lower.tail = TRUE, format = c("vector", "df")) {
  stopifnot(!is.null(p) && !anyNA(p))
  if (any(p < 0 | p > 1)) stop("p must be between 0 and 1")

  format <- match.arg(format)
  # if (!lower.tail) {
  #   p <- 1 - p
  # }
  pql <- pql(0:.maxql, eav, skill, lower.tail = lower.tail, format = format)

  if (format == "vector") {
    result <- findFirstGE(pql, p, forward = lower.tail)
    return(setNames(result, p))
  } else {
    result <- findFirstGE(pql$p, p, forward = lower.tail)
    pql <- pql[result,]
    pql$p <- p
    row.names(pql) <- p
    return(pql)
  }
  result
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




#' The likelihood for the outcomes of a skill check.
#'
#' @param s1,s2,s3 effective attribute
#' @details Uses a brute force algorithm. It is intended for analysis purposes
#' rather than practical use.
#' @returns A vector of length 3*20. Each place represent the
#' number of events for the sum of dice.
#' @export
#'
#' @examples
#' dSkill_BF(7, 10, 13)
dSkill_BF <- function(eav, skill) {
  stopifnot(length(eav) == 3L)
  stopifnot(all(eav > 0))
  stopifnot(skill >= 0)

  # Cap input parameters at `maxd20`
  s1 <- min(eav[1L], maxd20)
  s2 <- min(eav[2L], maxd20)
  s3 <- min(eav[3L], maxd20)
  totalPoints <- s1 + s2 + s3 + skill

  # Generate all 8,000 combinations of three D20 rolls (1:20) as a matrix
  rolls <- expand.grid(a = 1:maxd20, b = 1:maxd20, c = 1:maxd20, KEEP.OUT.ATTRS = FALSE)

  # Vectorized check: TRUE when at least two dice show 1 (critical failure condition)
  # two_ones <- (rolls$a == 1L & rolls$b == 1L) |
  #   (rolls$b == 1L & rolls$c == 1L) |
  #   (rolls$a == 1L & rolls$c == 1L)
  two_ones <- rowSums(rolls == 1L) > 1L
  two_twenties <- rowSums(rolls == 20L) > 1L

  # DEBUG
  stopifnot(sum(two_ones) == 58L)
  stopifnot(sum(two_twenties) == 58L)

  # Compute sums efficiently:
  # - Critical failure: use fixed sum of lower bounds
  # - Otherwise: sum of each roll clamped to its respective lower bound
  sums <- ifelse(
    two_ones,
    skill,
    ifelse(
      two_twenties,
      -1,
      totalPoints - (pmax(rolls$a, s1) + pmax(rolls$b, s2) + pmax(rolls$c, s3))
      )
    )

  # Tabulate frequencies into a vector of length 60 (sums 1-60)
  # tabulate() automatically bins integer values 1:nbins with 1-based indexing
  result <- table(sums)
  Negatives <- sum(result[names(result) < 0])
  result <- result[names(result) >= 0]
  result <- c(`-x` = Negatives, result)

    #---tabulate(sums, nbins = 3L * maxd20 + 1L)
  return(result)
}
