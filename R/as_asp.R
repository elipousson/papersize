#' Convert character string, page name, or page data.frame to numeric aspect ratio
#'
#' @param asp Aspect ratio of width to height as a numeric value (e.g. 0.33) or
#'   characters separated by a colon (e.g. "1:3"). If numeric, [as_asp()]
#'   returns the value of asp without modification.
#' @param page If character, page is passed as the name parameter to
#'   [get_page_size()].
#' @inheritParams get_page_size
#' @inheritDotParams get_page_size
#' @seealso [as_orientation()]
#' @examples
#' as_asp(8.5 / 11)
#'
#' as_asp("11:17")
#'
#' as_asp(paper = "letter")
#'
#' as_asp(paper = "letter", orientation = "landscape")
#'
#' @export
as_asp <- function(asp = NULL,
                   page = NULL,
                   orientation = NULL,
                   ...) {
  if (is.null(asp) & is.null(page)) {
    return(asp)
  }

  if (is.character(asp)) {
    w <- as.numeric(str_extract(asp, ".+(?=:)"))
    h <- as.numeric(str_extract(asp, "(?<=:).+"))
    asp <- w / h
  }

  if (is.numeric(asp)) {
    return(asp)
  }

  asp_col <- get_asp_col()

  if (is.data.frame(page)) {
    return(page[[asp_col]])
  }

  if (is.character(page)) {
    page <- get_page_size(page, orientation = orientation, ...)
    return(page[[asp_col]])
  }
}
