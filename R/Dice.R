

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
newRoll <- function(c, f, m = 0) {
  stopifnot(
    is.numeric(c), c > 0, c %% 1 == 0,
    is.numeric(f), f > 1, f %% 1 == 0,
    is.numeric(m), m %% 1 == 0
  )
  result <- c(Count = c, Faces = f, Mod = m)
  class(result) <- "Roll"
  return(result)
}

#' @describeIn newRoll S3 method for class 'Roll'
#' @export
is.Roll <- function(x)
  is.atomic(x) &&
  isa(x, "Roll") &&
  length(x) == 3L &&
  all(names(x) == c("Count", "Faces", "Mod"))


#' @describeIn newRoll S3 method for class 'Roll'
#' @export
print.Roll <- function(x) {
  if (x[3] > 0L)
    s <- "+"
  else
    s <- "-"
  sprintf("%dd%d%s%d", x[1], x[2], s, abs(x[3])) |>
    cat()
  invisible(x)
}



#' Creates an object to specify a
#' combination of dice with count and faces.
#' @param c The dice count (integer > 0)
#' @param f The faces/sides of the dice (integer > 1)
#' @param m an additive modifier (integer)
#'
#' @returns a `Roll` object is a named vector with
#' count and faces.
#' @export
#' @examples
#' newDice(2, 20)
newDice <- function(c, f) {
  stopifnot(
    is.numeric(c), c > 0, c %% 1 == 0,
    is.numeric(f), f > 1, f %% 1 == 0
  )
  result <- c(Count = c, Faces = f)
  class(result) <- "Dice"
  return(result)
}

#' @describeIn newDice S3 method for class 'Dice'
#' @param x a Dice object
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
    cat()
  invisible(x)
}



#dice_strings <- c("2d6", "3d20", "10d8", "1d100", "roll 4d12 now")

#' @returns Either a single vector with length 2
#' and the names "count" and "face" or a list of those.
#' It depends on the input and if there is more than 1 die
#' found.
d <- function(x) {
  if (is.character(x)) {
    pattern <- "(\\d+)d(\\d+)" #"([1-9]\\d*)?d([1-9]\\d*)([/x][1-9]\\d*)?([+-]\\d+)?"
    matches <- regmatches(dice_strings, regexec(pattern, dice_strings))

    dice <- do.call(rbind, lapply(matches, function(m) {
      if (length(m) == 3) {
        data.frame(
          num_dice = as.integer(m[2]),
          num_sides = as.integer(m[3])
        )
      } else {
        stop("String cannot be interpreted as dice")
      }
    }))
    return (dice)
  }
  if (is.numeric(x)) {

  }
}
