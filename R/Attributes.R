
#' @describeIn cAttr Uses a brute force algorithm. It is
#' intended for analysis purposes rather than practical use.
cAttr_BF <- function(eav) {
  stopifnot(eav > 0)

  result <- list(
    Critical = rep(0.0, length(eav)),
    Success = rep(0.0, length(eav)),
    Fail = rep(0.0, length(eav)),
    Botch = rep(0.0, length(eav))
  )

  TotalEvents <- 0.0
  for(roll in 1L:20) {
    if (roll == 1L) { # potential critical
      for (confirmRoll in 1L:20) {
        TotalEvents <- TotalEvents + 0.05
        testCritical <- confirmRoll <= eav & confirmRoll < 20L
        result$Critical <- result$Critical + ifelse(testCritical, 0.05, 0)
        result$Success <- result$Success + ifelse(testCritical, 0, 0.05)
      }
    }
    else if (roll == 20L) { # potential botch
      for (confirmRoll in 1L:20) {
        TotalEvents <- TotalEvents + 0.05
        testBotch <- confirmRoll > eav | confirmRoll == 20L
        result$Botch <- result$Botch + ifelse(testBotch, 0.05, 0)
        result$Fail <- result$Fail + ifelse(testBotch, 0, 0.05)
      }
    }

    else { # regular roll
      TotalEvents <- TotalEvents + 1L
      testCheck <- roll <= eav & roll < 20L
      result$Success <- result$Success + ifelse(testCheck, 1, 0)
      result$Fail <- result$Fail + ifelse(testCheck, 0, 1)
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

