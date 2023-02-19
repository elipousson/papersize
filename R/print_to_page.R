#' Explicitly draw plot using dimensions from page data.frame or list
#'
#' @param plot Plot to display
#' @inheritParams ggplot2::print.ggplot
#' @inheritParams page_to_viewport
#' @inheritDotParams page_to_viewport
#' @examples
#' \dontrun{
#' if (interactive() & is_installed("ggplot2")) {
#'   library(ggplot2)
#'
#'   plot <-
#'     ggplot(mpg, aes(displ, hwy, colour = class)) +
#'     geom_point()
#'
#'   print_to_page(
#'     plot,
#'     page = get_page_size("Tarot card")
#'   )
#' }
#' }
#' @returns Invisibly returns the original plot using a viewport from
#'   [page_to_viewport()].
#' @seealso [ggplot2::print.ggplot()]
#' @export
print_to_page <- function(plot, page, newpage = TRUE, vp = NULL, ...) {
  check_ggplot(plot)
  print(
    plot,
    newpage = newpage,
    vp = vp %||% page_to_viewport(page, ...)
  )
}

#' @rdname print_to_page
#' @name print_to_page_layout
#' @param layout Passed to page_to_viewport with layout_position as
#'   layout.pos.row and layout.pos.col if provided. Defaults to `NULL` where
#'   layout is defined by page_to_layout using ncol, nrow, page and any
#'   additional parameters passed to ....
#' @inheritParams page_to_layout
#' @inheritDotParams page_to_layout
#' @export
print_to_page_layout <- function(plot,
                                 page,
                                 row_position = 1,
                                 col_position = 1,
                                 col_width = NULL,
                                 row_height = NULL,
                                 nrow = 1,
                                 ncol = 1,
                                 layout = NULL,
                                 parent = NULL,
                                 children = NULL,
                                 filename = NULL,
                                 ...) {
  nrow <- max(nrow, row_position)
  ncol <- max(ncol, col_position)

  layout <- layout %||% page_to_layout(
    page,
    ncol = ncol,
    nrow = nrow,
    ...
  )

  parent <-
    parent %||%
    page_to_viewport(
      page,
      layout = layout
    )

  children <-
    children %||% grid::vpList(
      grid::viewport(
        name = paste0(paste0("Row", row_position), paste0("Col", col_position)),
        layout.pos.row = row_position,
        layout.pos.col = col_position
      )
    )

  page_vp <-
    grid::vpTree(
      parent = parent,
      children = children
    )

  print_to_page(
    plot,
    page,
    vp = page_vp
  )

  if (is.null(filename)) {
    return(invisible())
  }

  ggplot2::ggsave(
    filename = filename,
    width = page[["width"]],
    height = page[["height"]],
    units = page[["units"]]
  )
}

#' @noRd
get_region_dims <- function(page,
                            cols = NULL,
                            rows = NULL,
                            layout = NULL,
                            ncol = 1,
                            nrow = 1,
                            ...) {
  layout <- layout %||% page_to_layout(
    page,
    ncol = max(cols, ncol),
    nrow = max(rows, nrow),
    ...
  )

  c(
    sum_num(layout[["widths"]][cols %||% 1]),
    sum_num(layout[["heights"]][rows %||% 1])
  )
}

#' @noRd
check_ggplot <- function(plot, class = NULL) {
  check_installed("ggplot2")
  check_required(plot)

  message <- "{.arg plot} must be a {.cls ggplot} object."
  inherits_class <- FALSE

  if (!is.null(class)) {
    message <- "{.arg plot} must be a {.cls ggplot} object or an object with class {.cls {class}}."
    inherits_class <- rlang::inherits_any(plot, class)
  }

  cli_abort_ifnot(
    message = message,
    condition = ggplot2::is.ggplot(plot) | inherits_class
  )
}
