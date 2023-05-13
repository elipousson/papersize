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
#' @param dims Optional. Plot dimensions. Ignored if ncol and nrow are supplied.
#'   Otherwise, if `NULL` (default), dims are inferred based on the dimensions
#'   of the first plot in plots.
#' @param ncol,nrow The dimensions of the grid to create.  If both are `NULL`,
#'   dims will be used or dims will be determined based on the plot dimensions.
#' @param images  Not yet implemented. If `TRUE` and dims is `NULL`, the input
#'   plots are assumed to be plots created with [magick::image_ggplot()] and dpi
#'   is used to infer dimensions.
#' @param dpi Not yet implemented. Resolution.
#' @inheritParams rlang::args_error_context
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
                        ncol = NULL,
                        nrow = NULL,
                        dims = NULL,
                        images = FALSE,
                        dpi = 120,
                        call = caller_env()) {
  check_installed(c("ggplot2", "patchwork"), call = call)

  page_grid <-
    set_page_grid(
      plots = plots,
      page = page,
      width = width,
      height = height,
      orientation = orientation,
      dims = dims,
      ncol = ncol,
      nrow = nrow,
      images = images,
      dpi = dpi
    )

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

#' @noRd
set_page_grid <- function(plots = NULL,
                          page = NULL,
                          ncol = NULL,
                          nrow = NULL,
                          dims = NULL,
                          images = FALSE,
                          dpi = 120,
                          ...,
                          call = caller_env()) {
  if (!is_null(ncol) && !is_null(nrow)) {
    if (!is_null(dims)) {
      cli_alert_warning(
        "{.arg dims} is ignored if {.arg ncol} and {.arg nrow} are supplied."
      )
    }

    check_number_whole(ncol)
    check_number_whole(nrow)
    return(c(ncol, nrow))
  }

  page_dims <- get_page_dims(page, ...)

  if (!is_null(dims)) {
    if (is.data.frame(dims)) {
      dims <- get_page_dims(dims)
    } else if (is_character(dims)) {
      dims <- get_page_dims(get_page_size(dims))
    } else if (!is_bare_numeric(dims)) {
      cli::cli_abort(
        "A {.arg dims} must be a a {.cls data.frame} with plot dimensions,
        a {.cls character} string with the name of a paper size, or a
        {.cls numeric} object with plot width and height.",
        call = call
      )
    }

    return(as.numeric(page_dims %/% dims))
  }

  dims_plot <- plots[[1]]
  cli::cli_alert_info(
    "Using {.arg dims} from first plot in {.arg plots}."
  )

  if (!images) {
    plot_data <- ggplot2::layer_data(dims_plot)
  } else if (images && has_annotation(dims_plot)) {
    # FIXME: This method works interactively but not inside of a function.
    # plot_data <- ggplot2::ggplot_build(dims_plot)$layers[[2]]$computed_geom_params
    # plot_data <- dims_plot$layers[[2]]$computed_geom_params
    # plot_data <- c(
    #   "xmin" = plot_data$xmin,
    #   "xmax" = plot_data$xmax,
    #   "ymin" =  plot_data$ymin,
    #   "ymax" = plot_data$ymax
    #   )
    # stopifnot(is.numeric(dpi))
    # plot_data <- as.list(plot_data / dpi)
  }

  cli_ifnot(
    "{.arg dims} can't be determined from {.arg plot} and must be supplied.",
    condition = all(has_name(plot_data, c("xmin", "xmax", "ymin", "ymax"))),
    .fn = cli::cli_abort,
    call = call
  )

  dims <-
    c(
      "width" = abs(diff(c(plot_data$xmin, plot_data$xmax))),
      "height" = abs(diff(c(plot_data$ymin, plot_data$ymax)))
    )

  as.numeric(page_dims %/% dims)
}
