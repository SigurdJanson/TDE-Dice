.checkTypes <- c("Attack", "Parry", "Dodge", "Skill", "Attribute")

#' Creates an object to specify the CSFB outcomes of a
#' TDE check. CSFB are the 4 values of this check
#' which are *c*ritical successes, regular *s*uccesses,
#' *f*ails, and *b*otches
#' @param cr,su,fa,bo numeric vectors specifying the probabilities
#' for criticals, successes, fails, and botches.
#' @param check The type of check ("Attack", "Parry", "Dodge",
#' "Skill", or "Attribute").
#'
#' @returns a `CSFB` object is a list with
#' the four probability vectors and a string to identify the
#' type of the check.
#'
#' @export
#'
#' @examples
#' newCSFB(0.05, 0.45, 0.45, 0.05, "Attack")
newCSFB <- function(cr, su, fa, bo, check = .checkTypes) {
  if (any(cr < 0) || any(su < 0) || any(fa < 0) || any(bo < 0) )
    stop("Probabilities must be >= 0")
  if (any(cr > 1) || any(su > 1) || any(fa > 1) || any(bo > 1) )
    stop("Probabilities must be <= 1")

  check <- match.arg(check)

  result <- structure(
    list(
      Critical = cr,
      Success = su,
      Fail = fa,
      Botch = bo
    ),
    class ="CSFB", check = check
  )
  if (any(diff(lengths(result))) != 0)
    stop("Unequal number of checks")

  return(result)
}

#' @describeIn newCSFB S3 method for class 'CSFB'
#' @param x object of class `CSFB`.
#' @export
is.CSFB <- function(x)
  is.list(x) &&
  isa(x, "CSFB") &&
  attr(x, "check") %in% .checkTypes &&
  length(x) == 4L &&
  all(names(x) == c("Critical", "Success", "Fail", "Botch"))


#' @describeIn newCSFB S3 method for class 'CSFB'
#' @param x object of class `CSFB`.
#' @param digits the minimum number of significant digits to be used:
#' see print.default.
#' @param quote logical, indicating whether or not entries should
#' be printed with surrounding quotes.
#' @param right logical, indicating whether or not strings should be
#' right-aligned. The default is right-alignment.
#' @param row.names logical (or character vector), indicating whether
#' (or what) row names should be printed.
#' @param max numeric or NULL, specifying the maximal number of rows
#' to be printed. By default, when NULL, getOption("max.print") used.
#' @param ... further arguments to be passed from or to other methods.
#'
#' @returns
#' @export
#'
#' @examples
#' print(newCSFB(0.05, 0.45, 0.45, 0.05, "D"))
print.CSFB <- function(x, digits = NULL, quote = FALSE,
                       right = TRUE, row.names = FALSE, max = 6L, ...) {
  if (is.numeric(max)) max <- 4*max

  cstr <- if (length(x[[1]]) != 1) "checks" else "check"
  paste(length(x[[1]]), attr(x, "check"), cstr, "\n") |>
    cat()

  unclass(x) |> data.frame() |>
    print(digits=digits, quote=quote, right=right,
          row.names=row.names, max=max, zero.print = ".", ...)

  sums <- sapply(seq_along(x[[1]]), \(i) sum(sapply(x, `[[`, i)))
  if (any(sums != 1))
    cat("\nNote: Object is irregular. Not all probabilities add up to 1.")

  invisible(x)
}
