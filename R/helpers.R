
#' findFirstGE
#'
#' Find the first index `i` in `x` for which `x[i] >= target` holds true.
#' @param x A numeric vector with monotonously increasing values.
#' @param target A numeric vector.
#' @details This is a helper function for quantile functions.
#' It also works outside the value range [0, 1], but has been tested
#' most extensively within this range.
#' @returns For each target it returns the first `x[i]` (from left to right)
#' for which `x[i] >= target` holds true.
#' If `target` is greater than any `x`, it returns `NA`.
Rcpp::cppFunction('
  IntegerVector findFirstGE(NumericVector x, NumericVector target) {
    int xn = x.size();
    if (xn == 0) return Rcpp::IntegerVector();
    int tn = target.size();

    Rcpp::IntegerVector result(tn, NA_INTEGER);
    int totalFound = 0;

    for (int i = 0; i < xn; i++) {
      if (totalFound == tn) return result;

      for (int t = 0; t < tn; t++) {
        if (IntegerVector::is_na(result[t]) && x[i] >= target[t]) {
          if (!NumericVector::is_na(target[t]))
            result[t] = i + 1; // R is 1-indexed
          totalFound++;
        }
      }
    }
    return result;
  }
')


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




#
# CONVOLUTION ##################
#

#' convolveDice
#'
#' Convolves two distributions of [n]d[M].
#'
#' @param x,y The two vectors to convolve.
#'
#' @returns Vector of length `length(x) + length(y)`.
convolveDice <- function(x, y) {
  result <- integer(length(x) + length(y))
  for (a in 1:length(x)) {
    sum <- a + 1:length(y)
    prob <- x[a] * y
    result[sum] <- result[sum] + prob
  }
  return(result)
}
