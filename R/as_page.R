#' Coerce a character or named vector to a page data.frame using get_page_size()
#' or make_page_size()
#'
#' @param x A character vector passed to the name parameter of
#'   [get_page_size()], a named list of vector passed to dims for
#'   [make_page_size()], or a length 2 numeric vector passed as the width and
#'   height parameter to [make_page_size()]. If x is a character string that
#'   does not match any of the names in paper_sizes, x is passed as the name
#'   parameter to [make_page_size()]
#' @inheritParams make_page_size
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
#' @returns A data.frame or list object with one or more page dimensions.
#' @export
#' @importFrom rlang is_character is_named is_bare_numeric has_length list2
as_page <- function(x,
                    ...,
                    cols = c("width", "height"),
                    class = "data.frame") {
  if (is_character(x) & all(tolower(x) %in% tolower(paper_sizes[["name"]]))) {
    page <- get_page_size(x, ...)
    if (class == "list") {
      page <- page_to_list(page)
    }
  } else if (rlang::is_named(x)) {
    page <- make_page_size(dims = x, ..., cols = cols, class = class)
  } else if (is_bare_numeric(x) && has_length(x, 2)) {
    params <- rlang::list2(...)
    page <- make_page_size(
      width = x[[1]],
      height = x[[2]],
      units = params[["units"]],
      orientation = params[["orientation"]],
      name = params[["name"]],
      cols = cols,
      class = class
    )
  } else if (is_character(x)) {
    page <- make_page_size(name = x, ..., cols = cols, class = class)
  }

  page
}
