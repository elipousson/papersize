
#' Create an inset with page size dimensions be added on top of the previous plot
#'
#'
#' @param inset_page Page size data.frame to use for inset, Default: `NULL`
#' @param left,bottom,right,top numerics or units giving the location of the
#'   outer bounds. If given as numerics and inset_page is NULL, they will be
#'   converted to npc units. All four are required if inset_page is` NULL`. If
#'   inset_page is provided, top *or* bottom and left *or* right must be
#'   provided as the inset element is expected to be the width and height
#'   defined by inset_page.
#' @inheritParams patchwork::inset_element
#' @inheritDotParams convert_unit_type
#' @inherit patchwork::inset_element return
#' @examples
#' \dontrun{
#' if (interactive() & is_installed("ggplot2")) {
#'   library(ggplot2)
#'   p <- ggplot(mpg, aes(displ, fill = class)) +
#'     geom_bar()
#'
#'   ggplot(mpg, aes(displ, hwy, colour = class)) +
#'     geom_point() +
#'     inset_page_element(
#'       p = p,
#'       inset_page = get_page_size("Poker card", orientation = "landscape"),
#'       left = unit(1, "in"),
#'       bottom = unit(1, "in")
#'     )
#' }
#' }
#' @seealso
#'  \code{\link[patchwork]{inset_element}}
#' @rdname inset_page_element
#' @export
#' @importFrom rlang check_installed
inset_page_element <- function(p,
                               inset_page = NULL,
                               left = NULL,
                               bottom = NULL,
                               right = NULL,
                               top = NULL,
                               align_to = "panel",
                               on_top = TRUE,
                               clip = TRUE,
                               ignore_tag = FALSE,
                               ...) {
  if (!is.null(inset_page)) {
    pg_unit <- inset_page[[get_units_col()]]
    pg_w <- as_unit(inset_page[["width"]], pg_unit)
    pg_h <- as_unit(inset_page[["height"]], pg_unit)

    if (!is.null(left) & is.null(right)) {
      left <- convert_unit_type(left, to = pg_unit, ...)
      right <- left + pg_w
    }

    if (!is.null(bottom) & is.null(top)) {
      bottom <- convert_unit_type(bottom, to = pg_unit, ...)
      top <- bottom + pg_h
    }

    if (!is.null(right) & is.null(left)) {
      right <- convert_unit_type(right, to = pg_unit, ...)
      left <- right - pg_w
    }

    if (!is.null(top) & is.null(bottom)) {
      top <- convert_unit_type(top, to = pg_unit, ...)
      bottom <- top - pg_h
    }
  }

  rlang::check_installed("patchwork")
  patchwork::inset_element(
    p + theme_page(inset_page),
    left,
    bottom,
    right,
    top,
    align_to,
    on_top,
    clip,
    ignore_tag
  )
}
