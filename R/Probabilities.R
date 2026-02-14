#' Create a probability distribution for a d20 truncated at and
#' rectified to `eav`.
#' @param eav The effective attribute value of the check
#' @returns An integer vector representing the number of events
#' for each face from 1 to 20.
#' @details This function does not validate it's arguments.
#' It is only defined for `1 <= eav <= 20`.
rect1d20 <- function(eav) c(rep(0L, eav-1L), eav, rep(1L, 20L - eav))

#' The density function for rolling a sum of 3d20.
#' @param x A vector of dice sums.
#' @returns The vector of length 60 starts with indices 1 and 2,
#' both of which are zero because 3d20 has a sum of at least 3.
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
#' in a 3d20 which are defined by at least two instances
#' of a 1 in that roll.
#' @param eav effective attribute value
crit3d20 <- function(eav) {
  e1 <- c(rep(0, sum(eav)-1L), eav[1], rep(1, 20-eav[1]), rep(0, 40))
  e1 <- e1[1:60]
  e2 <- c(rep(0, sum(eav)-1L), eav[2], rep(1, 20-eav[2]), rep(0, 40))
  e2 <- e2[1:60]
  e3 <- c(rep(0, sum(eav)-1L), eav[3], rep(1, 20-eav[3]), rep(0, 40))
  e3 <- e3[1:60]
  e1[sum(eav)] <- e1[sum(eav)] -2L
  return(e1+e2+e3)
}

# @describeIn d3D20 Distribution of botches
#' in a 3d20 which are defined by at least two instances
#' of a 1 in that roll.
#' @param eav effective attribute value
botch3d20 <- function(eav) {
  e1 <- c(rep(0, 39+eav[1]), eav[1], rep(1, 20-eav[1]))
  e1 <- e1[1:60]
  e2 <- c(rep(0, 39+eav[2]), eav[2], rep(1, 20-eav[2]))
  e2 <- e2[1:60]
  e3 <- c(rep(0, 39+eav[3]), eav[3], rep(1, 20-eav[3]))
  e3 <- e3[1:60]
  e1[60L] <- e1[60L] -2L
  return(e1+e2+e3)
}


#
# ATTRIBUTES ###################
#


#' Determine the probability of an attribute check
#' @describeIn pAttr Uses a brute force algorithm. It is
#' intended for analysis purposes rather than practical use.
#' @param eav effective attribute value
pAttr_BF <- function(eav) {
  stopifnot(eav > 0)

  result <- list(
    Critical = 0.0,
    Success = 0.0,
    Fail = 0.0,
    Botch = 0.0
  )

  TotalEvents <- 0.0
  for(roll in 1L:20) {
    if (roll == 1L) { # potential critical
      for (confirmRoll in 1L:20) {
        TotalEvents <- TotalEvents + 0.05
        if (confirmRoll <= eav)
          result$Critical <- result$Critical + 0.05
        else
          result$Success <- result$Success + 0.05
      }
    }

    else if (roll == 20L) { # potential botch
      for (confirmRoll in 1L:20) {
        TotalEvents <- TotalEvents + 0.05
        if (confirmRoll <= eav && confirmRoll != 20L)
          result$Fail <- result$Fail + 0.05
        else
          result$Botch <- result$Botch + 0.05
      }
    }

    else { # regular roll
      TotalEvents <- TotalEvents + 1L
      if (roll <= eav)
        result$Success <- result$Success + 1L
      else
        result$Fail <- result$Fail + 1L
    }
  }

  return(
    list(
      Critical = result$Critical / TotalEvents,
      Success = result$Success / TotalEvents,
      Fail = result$Fail / TotalEvents,
      Botch = result$Botch / TotalEvents
    )
  )
}

#' Determine the probabilities for the outcomes of an attribute check
#' @param eav effective attribute value
#' @returns A list with 4 vectors: `Critical`, `Success`, `Fail`, `Botch`.
#' @export
#' @examples pAttr(12)
pAttr <- function(eav) {
  stopifnot(eav > 0)
  eav <- pmin(eav, 19)
  return(
    list(
      Critical = eav / 400.0,
      Success = 19*eav / 400.0,
      Fail = (380 - 19*eav) / 400.0,
      Botch = (20 - eav) / 400.0
    )
  )
}


pCombat <- function() {

}

#
# SKILLS ###################
#


#' The likelihood for the outcomes of a skill check.
#' @param x A vector of dice sums.
#' @param eav A vector with 3 places, each being an effective
#' attribute value.
#' @param skill The skill value
#' @param format Determines how the output is generated, one of
#' @returns The result depends on the `format` argument.
#' \describe{
#'   \item{class}{A list with 4 vectors: `Critical`, `Success`, `Fail`, `Botch`.}
#'   \item{df}{A data frame with the columns:
#'   Outcome (each sum of dice), `p` (probability for this outcome),
#'   `Remainder` (remaining skill points), `QL` (the quality level)}
#'   \item{ql}{List with all quality levels (`QL1`-`QL6`)
#'   and a list element for failures (`FAIL`).}
#' }
#' Each vector lists the probabilities for the given `x`.
#' @export
dSkill <- function(x, eav, skill, format = c("class", "df", "ql")) {
  stopifnot(length(eav) == 3L)
  stopifnot(all(eav > 0L))
  format <- match.arg(format)

  # "constants"
  maxd20 <- 20L
  max3d20 <- 60L # "constant': the max sum of 3d20
  totalEvents <- 8000L # "constant': number of events with 3d20

  #
  eav <- pmin(eav, maxd20)

  # Create the rectified 1d20 distributions and convolute them
  # into one distributions
  distr <- convolveDice(rect1d20(eav[1]), rect1d20(eav[2]))
  distr <- convolveDice(distr, rect1d20(eav[3]))
  distr <- distr / totalEvents

  distr <- distr -
    crit3d20(eav) / 8000 -
    botch3d20(eav) / 8000

  # EXTRACT RESULTS
  remainingSkillPoints <- if (skill > 0) (skill-1):0 else integer()
  remainingSkillPoints <- c(rep(skill, sum(eav)), remainingSkillPoints)[1:max3d20]
  if (length(remainingSkillPoints) < max3d20)
    remainingSkillPoints <- c(remainingSkillPoints, rep(0L, max3d20-length(remainingSkillPoints)))

  data <- data.frame(
    p = distr,
    Outcome = 1L:max3d20,
    Remainder = factor(remainingSkillPoints),
    QL = factor(qualityLevel(remainingSkillPoints),
                levels = c(1:qualityLevel(skill), "0"),
                labels = c(paste0("QL", 1:qualityLevel(skill)), "Failed"))
  )

  # FORMAT OUTPUT
  if (format == "class") {
    threshold <- min(sum(eav) + skill, max3d20) # separates success from failures
    return(
      list(
        Critical = (3*19 + 1) / totalEvents,
        Success = sum(distr[1:threshold]),
        Fail = if (threshold < max3d20)
          sum(distr[(threshold+1L):max3d20])
        else
          0.0,
        Botch = (3*19 + 1) / totalEvents
      )
    )
  } else if (format == "df") {
    return(data)
  } else if (format == "ql") {
    stop("not implemented")
  }
}

