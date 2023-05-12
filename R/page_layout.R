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
#' @param paginate If `TRUE`, create a list of `patchwork` objects when the
#'   number of plots is greater than the number of spaces in the plot layout.
#'   Default to `TRUE`.
#' @param dims Optional. Plot dimensions. If `NULL` (default), dims are inferred
#'   based on the dimensions of the first plot in plots.
#' @param images If `TRUE` and dims is `NULL`, the input plots are assumed to be
#'   plots created with [magick::image_ggplot()] and dpi is used to infer
#'   dimensions.
#' @param dpi If images is `TRUE` and dims is `NULL`,
#' @param ... Additional parameters passed to [get_page_size()] if dims is a
#'   character object.
#' @return A `patchwork` object or a list of `patchwork` objects.
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
                        images = FALSE,
                        dpi = 120,
                        ...) {
  check_installed(c("ggplot2", "patchwork"))

  page_dims <- get_page_dims(page, width, height, orientation)

  if (!is_null(dims)) {
    if (is.data.frame(dims)) {
      dims <- get_page_dims(dims)
    } else if (is_character(dims)) {
      dims <- get_page_dims(get_page_size(dims, ...))
    } else if (!is_bare_numeric(dims)) {
      cli::cli_abort(
        "A {.arg dims} must be a a {.cls data.frame} with plot dimensions,
        a {.cls character} string with the name of a paper size, or a
        {.cls numeric} object with plot width and height."
      )
    }
  } else {
    dims_plot <- plots[[1]]

    if(!images) {
      plot_data <- ggplot2::layer_data(dims_plot)
    } else if (images && has_annotation(dims_plot)) {
      plot_data <- dims_plot$layers[[2]]$computed_geom_params
      plot_data <- c(
        "xmin" = plot_data$xmin,
        "xmax" = plot_data$xmax,
        "ymin" =  plot_data$ymin,
        "ymax" = plot_data$ymax
        )
      stopifnot(is.numeric(dpi))
      plot_data <- as.list(plot_data / dpi)
    }

    dims <-
      c(
        "width" = abs(diff(c(plot_data$xmin, plot_data$xmax))),
        "height" = abs(diff(c(plot_data$ymin, plot_data$ymax)))
      )
  }

  page_grid <- as.numeric(page_dims %/% dims)

  stopifnot(all(page_grid > 0))

  if (is_null(plots)) {
    patch_layout <-
      patchwork::plot_layout(
        ncol = page_grid[[1]],
        nrow = page_grid[[2]],
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
        ncol = page_grid[[1]],
        nrow = page_grid[[2]],
        byrow = byrow,
        guides = guides,
        tag_level = tag_level,
        design = design
      )

    return(patch_layout)
  }

  plot_spaces <- page_grid[[1]] * page_grid[[2]]

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
        ncol = page_grid[[1]],
        nrow = page_grid[[2]],
        guides = guides,
        tag_level = tag_level,
        design = design
      )
    }
  )
}
