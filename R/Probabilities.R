#' Create a probability distribution for a d20 truncated at and
#' rectified to `eav`.
#' @param eav The effective attribute value of the check
#' @returns An integer vector representing the number of events
#' for each face from 1 to 20.
#' @details This function does not validate it's arguments.
#' It is only defined for `1 <= eav <= 20`.
rect1d20 <- function(eav) c(rep(0L, eav-1L), eav, rep(1L, 20L - eav))

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
#
# SKILLS ###################
#

#' The density function for rolling a sum of 3d20.
#' @param x A vector of dice sums.
#' @value The vector of length 60 starts with indices 1 and 2,
#' both of which are zero because 3d20 has a sum of at least 3.
d3D20 <- function(x) {
  freq <- c(0, 0, 1, 3, 6, 10, 15, 21, 28,
         36, 45, 55, 66, 78, 91, 105, 120, 136, 153,
         171, 190, 210, 228, 244, 258, 270, 280, 288, 294,
         298, 300, 300, 298, 294, 288, 280, 270, 258, 244,
         228, 210, 190, 171, 153, 136, 120, 105, 91, 78,
         66, 55, 45, 36, 28, 21, 15, 10, 6, 3, 1)

  if (missing(x)) return(p)
  if (missing(x)) return(freq / 8000)

  x[x < 3L | x > 60L] <- 1L # handle indices of p that are out of range
  return(freq[x] / 8000)
}


  x[x < 3L | x > 60L] <- 1L # handle indices of p that are out of range
  return(p[x] / 8000)
}

