#' Convert a page data.frame to a `viewport` class object
#'
#' Create a `viewport` class object with a width and height matching the
#' dimensions of a page data.frame and default.units that match the page units.
#'
#' @param page A page data.frame from [get_page_size()] or [make_page_size()].
#' @inheritParams grid::viewport
#' @inheritParams make_page_size
#' @inheritDotParams grid::viewport -default.units -width -height
#' @examples
#' vp <- page_to_viewport(get_paper("Poker card"))
#'
#' grid::grid.show.viewport(vp)
#'
#' @returns A `viewport` class object with the same width and height as the
#'   input page size.
#' @export
#' @importFrom rlang check_required
#' @importFrom grid viewport
page_to_viewport <- function(page,
                             name = NULL,
                             cols = c("width", "height"),
                             ...) {
  rlang::check_required(page)
  check_page(page, cols, n = 1)

  grid::viewport(
    name = name %||% page[["name"]],
    width = page[[cols[1]]],
    height = page[[cols[2]]],
    default.units = page[[get_units_col()]],
    ...
  )
}
