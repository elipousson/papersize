#' Set margins for page paper with margin
#'
#' @param margins Passed to [get_margin()] with unit
#' @export
set_page_margin <- function(page = NULL,
                            margins = NULL,
                            unit = "in",
                            cols = c("width", "height"),
                            ...) {
  page <- as_page(page, ..., cols = cols)

  margin <- get_margin(margins, unit = unit)

  if (!is_same_unit_type(page[[get_units_col()]], margin)) {
    margin <- convert_unit_type(margin, from = unit, to = page$units)
  }

  width_margin <- sum_num(c(margin[2], margin[4]))
  height_margin <- sum_num(c(margin[1] + margin[3]))

  cli_abort_if(
    "Combined l and r margin value ({width_margin}) must be less than
    the {.arg page} width ({page[[cols[1]]]})." = all(width_margin >= page$width)
  )

  cli_abort_if(
    "Combined t and b margin value ({height_margin}) must be less than
    the {.arg page} height ({page[[cols[2]]]})." = all(width_margin >= page$width)
  )

  body_width <- page$width - width_margin
  body_height <- page$height - height_margin

  nm <- c(
    names(page),
    paste0(get_body_col(), "_", c(cols, get_asp_col())),
    "margin"
  )

  page$body_width <- body_width
  page$body_height <- body_height
  page$body_asp <- page$body_width / page$body_height
  page$margin <- rep(NA, nrow(page))
  page$margin <- rep(list(margin), nrow(page))

  set_names(page, nm)
}
