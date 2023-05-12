#' @noRd
seq_number <- function(x,
                       start = NULL,
                       num_prefix = NULL,
                       num_suffix = NULL,
                       num_style = NULL,
                       pad = NULL,
                       side = "left",
                       base = 26) {

  num_style <- set_number_style(start, num_style)

  num <- set_start_number(seq_along(x), start, num_style)

  num <-
    switch(num_style,
      "arabic" = num,
      "alph" = int_to_alpha(num, base = base, dict = letters),
      "alpha" = int_to_alpha(num, base = base, dict = letters),
      "Alph" = int_to_alpha(num, base = base),
      "Alpha" = int_to_alpha(num, base = base),
      "roman" = int_to_roman(num, FALSE),
      "Roman" = int_to_roman(num)
    )

  if (!is.null(pad)) {
    num <- str_pad(num, max(nchar(num)), side, pad)
  }

  if (is.null(num_prefix) && is.null(num_suffix)) {
    return(num)
  }

  paste0(num_prefix, num, num_suffix)
}

#' @noRd
set_number_style <- function(start = NULL, num_style = NULL) {
  if (!is.null(num_style)) {
    num_style <-
      match.arg(
        num_style,
        c("arabic", "alph", "Alph", "alpha", "Alpha", "roman", "Roman")
      )

    return(num_style)
  }

  if (is.null(start) || is.integer(start)) {
    return("arabic")
  }

  if (start == "i") {
    return("roman")
  }


  if (start == "I") {
    return("Roman")
  }

  if (start %in% letters) {
    return("alpha")
  }

  if (start %in% LETTERS) {
    return("Alpha")
  }
}


#' Set start number for numeric vector x
#'
#' @noRd
set_start_number <- function(x, start = NULL, num_style = "arabic") {
  if (is.null(start)) {
    return(x)
  }

  if (!is.integer(start)) {
    if (any(num_style %in% c("alph", "Alph", "alpha", "Alpha"))) {
      start <- alpha_to_int(toupper(start))
    } else if (any(num_style %in% c("roman", "Roman"))) {
      start <- roman_to_int(start)
    } else {
      start <- suppressWarnings(as.integer(start))
    }

    stopifnot(is.integer(start))
  }


  x + (start - 1)
}

#' Convert a integer into a corresponding letter or multi-letter string
#'
#' Character values in the provided dict (default to letters "A" to "Z") are
#' passed as is. Non-integer numeric values or characters that are not found in
#' the provided dict are converting to NA values.
#'
#' @source Adapted from the recursive solution provided by G. Grothendieck in [a
#'   May 31, 2017 StackOverflow answer](https://stackoverflow.com/a/44274075).
#'
#' @param x An integer vector or a vector that can be coerced to an integer
#'   vector
#' @param suffix Suffix character to follow alpha character, e.g. if `x = 1` and
#'   `suffix = "."` the returned label would be "A.". suffix is also used to
#'   separate values when x is greater than base, e.g. `x = 27` and `suffix =
#'   "."` returns "A.A." Defaults to `NULL`.
#' @param base If base is not numeric, it is converted to an integer with
#'   [alpha_to_int()].
#' @param dict Character vector to compare to x. Default: LETTERS.
#' @param quiet If `TRUE`, suppress warnings for introduction of NA values
#'   through coercion.
#' @returns An integer vector composed of objects between 1 and 26 with the same
#'   length as x.
#' @noRd
int_to_alpha <- function(x,
                         suffix = NULL,
                         base = 26,
                         dict = LETTERS,
                         strict = FALSE) {
  if (length(x) > 1) {
    x <- vapply(
      x,
      int_to_alpha,
      NA_character_,
      suffix, base, dict, strict,
      USE.NAMES = FALSE
      )

    return(x)
  }

  x <- suppressWarnings(as.integer(x))

  if (strict) {
    stopifnot(!any(is.na(x)))
  } else if (is.na(x)) {
    return(NA_character_)
  }

  if (is.character(base)) {
    base <- alpha_to_int(base, dict = dict, strict = strict)
  }

  rest <- (x - 1) %/% base

  alpha <- paste0(dict[((x - 1) %% base) + 1], suffix)

  if (rest > 0) {
    return(Recall(rest, alpha, base, dict, strict))
  }

  alpha
}


#' Convert an alphabetical character object from A to Z into a corresponding
#' integer
#'
#' Integers and NA values are passed as is. Double or characters with no
#' corresponding Roman numeral are converting to NA values.
#'
#' @param x Character vector of length n strings to compare to dict. Typically,
#'   letters from "A" to "Z". Case sensitive.
#' @param dict Character vector to match to x. Default: LETTERS.
#' @param quiet If `TRUE`, suppress warnings for introduction of NA values
#'   through coercion.
##' @param call Default: [parent.frame()]. Passed to input checking functions
#'   to improve error messages.
#' @returns A length 1 integer between 1 and 26.
#' @noRd
alpha_to_int <- function(x,
                         dict = LETTERS,
                         strict = FALSE) {
  if (strict) {
    stopifnot(
      max(nchar(x)) == max(nchar(dict)),
      all(x %in% dict)
    )
  }

  as.integer(factor(x, levels = dict))
}

#' Convert a Roman numeral character object into a corresponding integer
#'
#' Integers and NA objects are passed as is. Double numeric objects or
#' characters with no corresponding Roman numeral are converting to NA values.
#'
#' @param x An integer vector or a character vector with characters representing
#'   Roman numerals.
#' @noRd
#' @importFrom utils as.roman
roman_to_int <- function(x, strict = FALSE) {
  x <- suppressWarnings(utils::as.roman(x))
  if (strict) {
    stopifnot(!any(is.na(x)))
  }

  as.integer(x)
}

#' @noRd
#' @importFrom utils as.roman
int_to_roman <- function(x, upper = TRUE, strict = FALSE) {
  x <- suppressWarnings(utils::as.roman(x))
  if (strict) {
    stopifnot(!any(is.na(x)))
  }

  if (!upper) {
    return(tolower(x))
  }

  as.character(x)
}
