#' Convert a page size data.frame to a Grid layout object
#'
#' @param page A page size data.frame from [get_page_size()] or a data.frame or
#'   list from [make_page_size()].
#' @param margins A numeric list or vector or a margin class object. Defaults to
#'   `NULL`. margins are removed from the overall layout width and height.
#' @param region Optional. An additional page data.frame where the region width
#'   and height are used as the column width and row height.
#' @param gutter Gutter width/height. Not yet implemented.
#' @param units Passed to default.units parameter of [grid::grid.layout()].
#' @inheritParams grid::grid.layout
#' @inheritParams make_page_size
#' @examples
#' a8_layout <-
#'   page_to_layout(
#'     get_paper("A8", orientation = "landscape"),
#'     ncol = 2,
#'     nrow = 2
#'   )
#'
#' grid::grid.show.layout(a8_layout)
#'
#' @returns A Grid layout object with the same width and height and default
#'   units as the input page size.
#' @export
#' @importFrom grid unit grid.layout
page_to_layout <- function(page,
                           margins = NULL,
                           region = NULL,
                           ncol = 1,
                           nrow = 1,
                           gutter = NULL,
                           widths = NULL,
                           heights = NULL,
                           units = "in",
                           respect = TRUE,
                           just = "center",
                           cols = c("width", "height")) {
  check_page(page, cols, n = 1)

  page_units <- as.character(page[[get_units_col()]])

  if (!is_same_unit_type(page_units, units)) {
    page <- convert_page_units(page, units)
  }

  page_width <- page[[cols[1]]]
  page_height <- page[[cols[2]]]

  if (!is_null(margins)) {
    page <- set_page_margin(page, margins, unit = units)
    body_cols <- glue("{get_body_col()}_{cols}")
    page_width <- page[[body_cols[1]]]
    page_height <- page[[body_cols[2]]]
  }

  widths <- widths %||% grid::unit(rep_len(page_width / ncol, ncol), units)
  heights <- heights %||% grid::unit(rep_len(page_height / nrow, nrow), units)

  if (!is_null(region)) {
    check_page(region, cols, n = 1)
    widths <- grid::unit(rep_len(region[[cols[1]]], ncol), units)
    heights <- grid::unit(rep_len(region[[cols[2]]], nrow), units)
  }

  if (sum_num(widths) > page_width) {
    cli_abort(
      c("Column widths must be less than {.arg page} width ({page_width}).",
        "i" = "Use a smaller {.arg ncol} value."
      )
    )
  }

  if (sum_num(heights) > page_height) {
    cli_abort(
      c("Row heights must be less than {.arg page} height ({page_height}).",
        "i" = "Use a smaller {.arg nrow} value."
      )
    )
  }

  grid::grid.layout(
    ncol = ncol,
    nrow = nrow,
    widths = widths,
    heights = heights,
    default.units = units,
    respect = respect,
    just = just
  )
}
