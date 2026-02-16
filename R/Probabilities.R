
# Skill related "constants"
maxd20 <- 20L # max faces of a d20
max3d20 <- 60L # the maximum sum of 3d20
totalEvents <- 8000L # total number of events with 3d20


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
#' in a 3d20 which are defined by at least two instances
#' of a 1 in that roll.
#' @param eav effective attribute value
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
#' in a 3d20 which are defined by at least two instances
#' of a 1 in that roll.
#' @param eav effective attribute value
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
  distr <- dSkill(1:60, eav, skill)

  return(newCSFB(
    cr = (3*19 + 1) / totalEvents,
    su = sum(distr[1:threshold]),
    fa = if (threshold < max3d20)
      sum(distr[(threshold+1L):max3d20])
    else
      0.0,
    bo = (3*19 + 1) / totalEvents,
    check = "Skill")
  )
}





#' The likelihood for the outcomes of a skill check.
#' @param x A vector of dice sums.
#' @param eav A vector with 3 places, each being an effective
#' attribute value.
#' @param skill The skill value
#' @param format Determines how the output is generated, one of
#'
#'
#' @returns The result depends on the `format` argument.
#' \describe{
#'   \item{vector}{}
#'   \item{df}{A data frame with the columns:
#'   Outcome (each sum of dice), `p` (probability for this outcome),
#'   `Remainder` (remaining skill points), `QL` (the quality level)}
#' }
#' Each vector lists the probabilities for the given `x`.
#' @export
dSkill <- function(x, eav, skill, format = c("vector", "df")) {
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
  distr <- distr - (crit3d20(eav) - botch3d20(eav)) / totalEvents

  # FORMAT OUTPUT
  if (format == "df") {
    if (skill > 0)
      skillRemainder <- (skill-1):0
    else
      skillRemainder <- integer()
    skillRemainder <- c(rep(skill, sum(eav)), skillRemainder)[1:max3d20]
    if (length(skillRemainder) < max3d20)
      skillRemainder <- c(skillRemainder, rep(0L, max3d20-length(skillRemainder)))

    data <- data.frame(
      p = distr,
      Outcome = 1L:max3d20,
      Remainder = factor(skillRemainder),
      QL = factor(qualityLevel(skillRemainder),
                  levels = c(1:qualityLevel(skill), "0"),
                  labels = c(paste0("QL", 1:qualityLevel(skill)), "Failed"))
    )

    return(data)
  } else {
    return(setNames(distr, 1L:max3d20))
  }
}

