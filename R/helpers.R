#' The break points needed to determine the quality level based on
#' a number of skill points.
qualityLevelBreakPoints <- c(4L, 7L, 10L, 13L, 16L)


#' Determine the quality level based on available skill points.
#' @param sp Skill points (integer)
#' @details This function does not validate its arguments.
#' It's been tested for a value range of [-20, 50].
qualityLevel <- function(sp)
  ifelse(sp >= 0L,
    findInterval(sp, vec = qualityLevelBreakPoints) + 1L,
    0L)



#' Get the width of bins when binning remaining skill points.
#' @param sp Skill points (scalar, integer)
#' @return A vector starting with the highest quality level.
#' For the current quality (i.e. the topmost one), it returns the
#' number of digits above the breakpoint below. Then, since the
#' lower quality are all fully included, it returns all bin sizes
#' of the lower bins in order from high to low.
binSkillPoints <- function(sp) {
  if (!is.numeric(sp) || any(sp < 0) || any(sp != floor(sp))) # || length(sp) != 1L
    stop("x must be a non-negative integer scalar")

  # Full bin sizes: bin1=4, bins2-5=3 each
  bin_sizes <- c(4L, 3L, 3L, 3L, 3L)

  ql <- findInterval(sp, qualityLevelBreakPoints) + 1L # determine quality level (1-6)
  if (ql == 1L)
    result <- integer()
  else
    result <- bin_sizes[1:ql]

  # Lower bound of current bin
  lower <- if (ql == 1L) 0L else qualityLevelBreakPoints[ql - 1L]

  # Count within current bin (inclusive)
  current_count <- sp - lower + 1L

  return(rev(c(result[1:length(result)-1], current_count)))
}




#' Convolutes two distributions of {n}d{M}.
convolveDice <- function(x, y) {
  result <- integer(length(x) + length(y))
  for (a in 1:length(x)) {
    for (b in 1:length(y))
    {
      sum <- a + b
      prob = x[a] * y[b];
      result[sum] = result[sum] + prob;
    }
  }
  return(result)
}

