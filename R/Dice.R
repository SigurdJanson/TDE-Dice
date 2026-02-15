

#' Creates an object to specify a die roll
#' with count, faces, and modifier.
#' @param c The dice count (integer > 0)
#' @param f The faces/sides of the dice (integer > 1)
#' @param m an additive modifier (integer)
#'
#' @returns a `Roll` object is a named vector with
#' count, faces, and modifier.
#' @export
#'
#' @examples
#' newRoll(2, 6, 2)
newRoll <- function(n, f, m = 0) {
  if (!(is.numeric(n) && n > 0 && n %% 1 == 0))
    stop("count must be an integer > 0")
  if (!(is.numeric(f) && f > 1 && f %% 1 == 0))
    stop("faces must be an integer > 1")
  stopifnot(
    is.numeric(m), m %% 1 == 0
  )
  result <- c(Count = n, Faces = f, Mod = m)
  class(result) <- "Roll"
  return(result)
}

#' @describeIn newRoll S3 method for class 'Roll'
#' @param x a `Roll` object
#' @export
is.Roll <- function(x)
  is.atomic(x) &&
  isa(x, "Roll") &&
  length(x) == 3L &&
  all(names(x) == c("Count", "Faces", "Mod"))


#' @describeIn newRoll S3 method for class 'Roll'
#' @param ... further arguments to be passed from or to other methods.
#' @export
print.Roll <- function(x, ...) {
  if (x[3] > 0L)
    s <- "+"
  else
    s <- "-"
  sprintf("%dd%d%s%d", x[1], x[2], s, abs(x[3])) |>
    cat(...)
  invisible(x)
}



#' Creates an object to specify a
#' combination of dice with count and faces.
#' @param c The dice count (integer > 0)
#' @param f The faces/sides of the dice (integer > 1)
#'
#' @returns a `Roll` object is a named vector with
#' count and faces.
#' @export
#' @examples
#' newDice(2, 20)
newDice <- function(n, f) {
  if (!(is.numeric(n) && n > 0 && n %% 1 == 0))
    stop("count must be an integer > 0")
  if (!(is.numeric(f) && f > 1 && f %% 1 == 0))
    stop("faces must be an integer > 1")

  result <- c(Count = n, Faces = f)
  class(result) <- "Dice"
  return(result)
}

#' @describeIn newDice S3 method for class 'Dice'
#' @param x a `Dice` object
#' @export
is.Dice <- function(x)
  is.atomic(x) &&
  isa(x, "Dice") &&
  length(x) == 2L &&
  all(names(x) == c("Count", "Faces"))

#' @describeIn newDice S3 method for class 'Dice'
#' @param ... further arguments to be passed from or to other methods.
#' @export
print.Dice <- function(x, ...) {
  sprintf("%dd%d", x[1], x[2]) |>
    cat(...)
  invisible(x)
}



#dice_strings <- c("2d6", "3d20", "10d8", "1d100", "roll 4d12 now")

#' d
#'
#' Coerces an object into a `Dice` object.
#'
#' @param x either a string in the format '\d*d\d+' or a vector
#' with 2 values.
#'
#' @details  If the vector is unnamed, the first value is
#' interpreted as number of dice and the second is the number
#' of sides. If there only one number it assumes `Count = 1`.
#' If the vector has names partial matching is used for
#' `c("Count", "Faces")`. Please note that partial matching
#' is case-sensitive.
#'
#' Do not add white space in strings.
#'
#' Decimal places are ignored when passing decimal numbers.
#'
#' @returns Either a single vector with length 2
#' and the names "count" and "face" or a list of those.
#' It depends on the input and if there is more than 1 die
#' found.
#'
d <- function(x) {
  if (is.character(x)) {
    stopifnot(`'d' converts one die at a time` = length(x) == 1L)

    pattern <- "(\\d*)d(\\d+)" #"([1-9]\\d*)?d([1-9]\\d*)([/x][1-9]\\d*)?([+-]\\d+)?"
    matches <- regmatches(x, regexec(pattern, x)) |>
      unlist()
    if (length(matches) == 3L) {
      if (nchar(matches[2]) == 0L)
        return(newDice(1L, as.integer(matches[3L])))
      else
        return(newDice(as.integer(matches[2L]), as.integer(matches[3L])))
    } else {
      stop("Dice format not recognized")
    }
    return (dice)
  }

  if (is.numeric(x)) {
    lenx <- length(x)
    if (length(names(x)) == 0) {
      stopifnot(`At least 1 value is required. Not more than 3.` =
                  lenx > 0 && lenx < 3)
      if (lenx == 1L)
        return(newDice(1L, as.integer(x[1L])))
      else
        return(newDice(as.integer(x[1L]), as.integer(x[2L])))
    } #unnamed
    # Named vector
    names(x) <- match.arg(names(x), c("Count", "Faces"), several.ok = TRUE)
    if (lenx == 1L)
      return(newDice(1L, as.integer(x["Faces"])))
    else
      return(newDice(as.integer(x["Count"]), as.integer(x["Faces"])))
  }

  stop("Dice format not recognized")
}
