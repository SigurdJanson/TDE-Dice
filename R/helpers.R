#' Determine the quality level based on available skill points.
#' @param sp Skill points (integer)
#' @details This function does not validate its arguments.
#' It's been tested for a value range of [-20, 50].
qualityLevel <- function(sp)
  ifelse(sp >= 0L,
    findInterval(sp, vec = c(4L, 7L, 10L, 13L, 16L)) + 1L,
    0L)

