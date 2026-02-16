
#' @describeIn cAttr Uses a brute force algorithm. It is
#' intended for analysis purposes rather than practical use.
cAttr_BF <- function(eav) {
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

  return(newCSFB(
      cr = result$Critical / TotalEvents,
      su = result$Success / TotalEvents,
      fa = result$Fail / TotalEvents,
      bo = result$Botch / TotalEvents,
      check = "Attribute")
  )
}



#' cAttr
#'
#' Determine the probabilities for the outcomes of an
#' attribute check.
#'
#' @param eav effective attribute value (integer, vector)
#' @returns A CSFB object with 4 vectors:
#' `Critical`, `Success`, `Fail`, `Botch`.
#'
#' @seealso \link{cCombat}, \link{cSkill}, \link{CSFB}
#'
#' @keywords distribution
#' @concept Univariate
#' @concept Discrete
#'
#' @export
#' @examples cAttr(12)
cAttr <- function(eav) {
  stopifnot(all(eav > 0))
  eav <- pmin(eav, 19)
  return(newCSFB(
    cr = eav / 400.0,
    su = 19*eav / 400.0,
    fa = (380 - 19*eav) / 400.0,
    bo = (20 - eav) / 400.0,
    check = "Attribute")
  )
}

