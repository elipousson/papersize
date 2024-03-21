#' Convert character string, page name, or page data.frame to numeric aspect
#' ratio
#'
#' Convert a character vector with an aspect ratio to a numeric aspect ratio or
#' get the aspect ratio for a given page that has an aspect ratio column.
#'
#' @param asp Aspect ratio of width to height as a numeric value (e.g. 0.33) or
#'   characters separated by a colon (e.g. "1:3"). If numeric, [as_asp()]
#'   returns the value of asp without modification.
#' @param page If character, page is passed as the name parameter to
#'   [get_page_size()].
#' @inheritParams get_page_size
#' @param flipped If `TRUE`, return aspect ratio of height to width (y / x),
#'   instead of width to height.
#' @param sep Separator character to use if asp is a character vector. Defaults
#'   to ":".
#' @inheritDotParams get_page_size
#' @seealso [isstatic::as_orientation()]
#' @returns A numeric vector.
#' @examples
#' as_asp(8.5 / 11)
#'
#' as_asp("11:17")
#'
#' as_asp("3/2", sep = "/")
#'
#' as_asp(page = "letter")
#'
#' as_asp(page = "letter", orientation = "landscape")
#'
#' as_asp(page = "letter", orientation = "portrait", flipped = TRUE)
#'
#' as_asp(page = make_page_size(8.5, 11, "in"), flipped = TRUE)
#'
#' @export
#' @importFrom rlang is_null is_string is_bare_numeric is_character has_name
as_asp <- function(asp = NULL,
                   page = NULL,
                   orientation = NULL,
                   flipped = FALSE,
                   sep = ":",
                   cols = c("width", "height"),
                   ...) {
  if (is_null(asp) && is_null(page)) {
    return(asp)
  }

  if (is_string(asp)) {
    w <- as.numeric(str_extract(asp, paste0(".+(?=", sep, ")")))
    h <- as.numeric(str_extract(asp, paste0("(?<=", sep, ").+")))
    asp <- w / h
  }

  if (is_bare_numeric(asp)) {
    return(set_flipped(asp, flipped))
  }

  if (is_character(page)) {
    page <- get_page_size(page, orientation = orientation, ...)
  }

  if (all(has_name(page, cols))) {
    asp <- as.numeric(page[[cols[1]]]) / as.numeric(page[[cols[2]]])
    return(set_flipped(asp, flipped))
  }

  asp_col <- get_asp_col()

  if (has_name(page, asp_col)) {
    return(set_flipped(page[[asp_col]], flipped))
  }
}

#' Helper function to optional switch orientation of aspect ratio
#'
#' @noRd
#' @importFrom rlang is_true
set_flipped <- function(x, flipped = FALSE) {
  if (is_true(flipped)) {
    return(1 / x)
  }

  x
}
