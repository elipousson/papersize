#' Coerce a character or named vector to a page data.frame using get_page_size()
#' or make_page_size()
#'
#' @param x A character vector passed to the name parameter of
#'   [get_page_size()], a named list of vector passed to dims for
#'   [make_page_size()], or a length 2 numeric vector passed as the width and
#'   height parameter to [make_page_size()]. If x is a character string that
#'   does not match any of the names in paper_sizes, x is passed as the name
#'   parameter to [make_page_size()]
#' @inheritDotParams make_page_size
#' @examples
#' as_page(c(8.5, 11), units = "in")
#'
#' as_page("letter")
#'
#' as_page(
#'   list(name = "letter", w = 8.5, h = 11, units = "in"),
#'   cols = c("w", "h")
#' )
#'
#' @export
as_page <- function(x, ..., cols = c("width", "height"), class = "data.frame") {
  if (is.character(x) & all(tolower(x) %in% tolower(paper_sizes[["name"]]))) {
    page <- get_page_size(x, ...)
  } else if (is_named(x)) {
    page <- make_page_size(dims = x, ..., cols = cols, class = class)
  } else if (is.numeric(x) & (length(x) == 2)) {
    page <- make_page_size(
      width = x[[1]],
      height = x[[2]],
      ...,
      cols = cols,
      class = class
    )
  } else if (is.character(x)) {
    page <- make_page_size(name = x, ..., cols = cols, class = class)
  }

  page
}
