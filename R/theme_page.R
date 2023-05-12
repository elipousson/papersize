#' Modify plot aspect ratio to match a page size
#'
#' Wrapper for ggplot2::theme() that passes as_asp() with `flip = TRUE` to the
#' aspect.ratio argument to force a plot to match the aspect ratio of the
#' provided page.
#'
#' @inheritParams as_asp
#' @inheritDotParams ggplot2::theme
#' @return A ggplot `theme` class object with the aspect.ratio argument set to
#'   match the page height / width.
#' @seealso
#'  [ggplot2::theme()]
#' @rdname theme_page
#' @export
theme_page <- function(page, orientation = NULL, ...) {
  check_installed("ggplot2")
  ggplot2::theme(
    aspect.ratio = as_asp(
      page = page,
      orientation = orientation,
      flipped = TRUE
    ),
    ...
  )
}
