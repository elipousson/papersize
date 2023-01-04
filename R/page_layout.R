#' Use patchwork to lay out a list of fixed aspect plots on a larger page
#'
#' @param plots Page name, a data.frame with width and height columns, or a list
#'   of ggplot2 objects with card plots. Default: `NULL`
#' @param page Paper name or a data.frame with width and height columns.
#'   Optional if width and height are both provided, Default: `NULL`
#' @param width,height Paper width and height, Default: `NULL`
#' @param orientation Paper orientation, Optional if width and height are both
#'   provided, Default: 'landscape'
#' @inheritParams patchwork::wrap_plots
#' @param ... Additional parameters passed to [get_paper_size()] if dims is a
#'   character object.
#' @return A `patchwork` object
#' @examples
#' page_layout(
#'   plots = plot_cards("Poker", 6),
#'   page = "letter"
#' )
#' @seealso
#'  [ggplot2::ggplot_build()]
#'  [patchwork::wrap_plots()], [patchwork::plot_layout()]
#' @rdname page_layout
#' @aliases layout_cards
#' @export
#' @importFrom ggplot2 layer_data
#' @importFrom patchwork wrap_plots plot_layout
page_layout <- function(plots = NULL,
                        page = NULL,
                        width = NULL,
                        height = NULL,
                        orientation = "landscape",
                        byrow = FALSE,
                        guides = NULL,
                        tag_level = NULL,
                        design = NULL,
                        paginate = TRUE,
                        dims = NULL,
                        ...) {
  page_dims <- get_page_dims(page, width, height, orientation)

  if (!is.null(dims)) {
    if (is.data.frame(dims)) {
      dims <- get_page_dims(dims)
    } else if (is.character(dims)) {
      dims <- get_page_dims(get_page_size(dims, ...))
    } else if (!is.numeric(dims)) {
      cli::cli_abort(
        "A {.arg dims} must be a a {.cls data.frame} with plot dimensions,
        a {.cls character} string with the name of a paper size, or a
        {.cls numeric} object with plot width and height."
      )
    }
  } else if (is_gg(plots[[1]])) {
    plot_data <- ggplot2::layer_data(plots[[1]])
    dims <-
      c(
        "width" = abs(diff(c(plot_data$xmin, plot_data$xmax))),
        "height" = abs(diff(c(plot_data$ymin, plot_data$ymax)))
      )
  }

  page_cols <- as.numeric(page_dims %/% dims)

  if (is.null(plots)) {
    patch_layout <-
      patchwork::plot_layout(
        ncol = page_cols[[1]],
        nrow = page_cols[[2]],
        byrow = byrow,
        guides = guides,
        tag_level = tag_level,
        design = design
      )

    return(patch_layout)
  }

  if (!paginate) {
    patch_layout <-
      patchwork::wrap_plots(
        plots,
        ncol = page_cols[[1]],
        nrow = page_cols[[2]],
        byrow = byrow,
        guides = guides,
        tag_level = tag_level,
        design = design
      )

    return(patch_layout)
  }

  plot_spaces <- page_cols[[1]] * page_cols[[2]]

  plots <-
    split(
      plots,
      ceiling(seq_along(plots) / plot_spaces)
    )

  map(
    plots,
    function(x) {
      patchwork::wrap_plots(
        x,
        ncol = page_cols[[1]],
        nrow = page_cols[[2]],
        guides = guides,
        tag_level = tag_level,
        design = design
      )
    }
  )
}
