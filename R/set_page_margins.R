#' Set margins for page data.frame (adding body width, height, and asp)
#'
#' @param page A character vector with a page size name or a data.frame. Passed
#'   to x parameter of [as_page()].
#' @param margins Passed to [get_margin()] with unit value.
#' @param unit Unit used for the margin. If margin is a unit object, unit is
#'   ignored. If page uses different units, the margins are converted into the
#'   page units for consistency.
#' @inheritParams make_page_size
#' @param ... Passed to [as_page()] with page and cols.
#' @returns A data.frame with the page dimensions and additional columns for
#'   body dimensions, body aspect ratio, and margins.
#' @seealso [ggplot2::margin()]; [set_page_dims()]; [set_page_orientation()];
#'   [set_page_asp()]
#' @export
set_page_margin <- function(page = NULL,
                            margins,
                            unit = "in",
                            cols = c("width", "height"),
                            ...) {
  page <- as_page(page, ..., cols = cols)
  rlang::check_required(margins)
  margin <- get_margin(margins, unit = unit)

  if (!is_same_unit_type(page[[get_units_col()]], margin)) {
    margin <- convert_unit_type(margin, from = unit, to = page$units)
  }

  width_margin <- sum_num(c(margin[2], margin[4]))
  height_margin <- sum_num(c(margin[1] + margin[3]))

  cli_abort_if(
    "Combined l and r margin value ({width_margin}) must be less than
    the {.arg page} width ({page[[cols[1]]]})." = all(width_margin >= page[[cols[1]]])
  )

  cli_abort_if(
    "Combined t and b margin value ({height_margin}) must be less than
    the {.arg page} height ({page[[cols[2]]]})." = all(height_margin >= page[[cols[2]]])
  )

  body_cols <- get_body_col(suffix = paste0("_", c(cols, get_asp_col())))

  nm <- c(
    names(page),
    body_cols,
    "margin"
  )

  page[[body_cols[1]]] <- page[[cols[1]]] - width_margin
  page[[body_cols[2]]] <- page[[cols[2]]] - height_margin
  page[[body_cols[3]]] <- page[[body_cols[1]]] / page[[body_cols[2]]]
  page[["margin"]] <- rep(NA, nrow(page))
  page[["margin"]] <- rep(list(margin), nrow(page))

  set_names(page, nm)
}
