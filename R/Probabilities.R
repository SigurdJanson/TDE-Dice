
#
# ATTRIBUTES ###################
#

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

#' AttributeCheck
#' @param eav effective attribute value
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
