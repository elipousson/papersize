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
#' @param margin Optional. A margin to add around the outside of the combined
#'   grid of plots, e.g. so the grid can be centered on a larger sheet of
#'   paper when saved with [ggplot2::ggsave()]. Passed to [get_margin()] with
#'   unit. The margin pads the composed page rather than the individual plots
#'   in `plots`, and does not affect the number of rows and columns in the
#'   grid. Default: `NULL`.
#' @param unit Unit used for `margin` if margin is a bare numeric vector or
#'   list. Ignored if margin is a `unit` class object. Default: `"in"`.
#' @param marks If `TRUE`, add crop marks in the `margin` area showing where
#'   to cut the page into individual plots. Requires `margin`. Marks are
#'   placed assuming the grid of plots exactly fills the page after
#'   subtracting `margin`, i.e. the same assumption `margin` itself relies
#'   on. Default: `FALSE`.
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
#'
#' page_layout(
#'   plots = plot_cards("Poker", 6),
#'   page = make_page_size(width = 2.5 * 3, height = 3.5 * 2, units = "in"),
#'   ncol = 3,
#'   nrow = 2,
#'   margin = margins(t = 0.75, r = 0.5, b = 0.75, l = 0.5, unit = "in"),
#'   marks = TRUE
#' )
#' @seealso
#'  [ggplot2::ggplot_build()]
#'  [patchwork::wrap_plots()], [patchwork::plot_layout()]
#' @rdname page_layout
#' @aliases layout_cards
#' @export
page_layout <- function(
  plots = NULL,
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
  margin = NULL,
  unit = "in",
  marks = FALSE,
  images = FALSE,
  dpi = 120,
  call = caller_env()
) {
  check_installed(c("ggplot2", "patchwork"), call = call)

  cli_abort_if(
    "{.arg marks} requires {.arg margin} (crop marks are drawn in the
    margin area)." = marks && is_null(margin)
  )

  page_grid <- set_page_grid(
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

  page_dims <- NULL
  if (marks) {
    page_dims <- get_page_dims(
      page,
      width = width,
      height = height,
      orientation = orientation
    )
  }

  if (is_null(plots)) {
    patch_layout <- patchwork::plot_layout(
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
    patch_layout <- patchwork::wrap_plots(
      plots,
      ncol = page_grid[[1]],
      nrow = page_grid[[2]],
      byrow = byrow,
      guides = guides,
      tag_level = tag_level,
      design = design
    )

    patch_layout <- add_page_margin(patch_layout, margin, unit = unit)

    if (marks) {
      patch_layout <- add_crop_marks(
        patch_layout,
        ncol = page_grid[[1]],
        nrow = page_grid[[2]],
        page_width = page_dims[["width"]],
        page_height = page_dims[["height"]],
        margin = get_margin(margin, unit = unit)
      )
    }

    return(patch_layout)
  }

  plot_spaces <- page_grid[[1]] * page_grid[[2]]

  plots <- split(
    plots,
    ceiling(seq_along(plots) / plot_spaces)
  )

  map(
    plots,
    function(x) {
      patch_layout <- patchwork::wrap_plots(
        x,
        ncol = page_grid[[1]],
        nrow = page_grid[[2]],
        guides = guides,
        tag_level = tag_level,
        design = design
      )

      patch_layout <- add_page_margin(patch_layout, margin, unit = unit)

      if (marks) {
        patch_layout <- add_crop_marks(
          patch_layout,
          ncol = page_grid[[1]],
          nrow = page_grid[[2]],
          page_width = page_dims[["width"]],
          page_height = page_dims[["height"]],
          margin = get_margin(margin, unit = unit)
        )
      }

      patch_layout
    }
  )
}

#' Add an outer margin around a composed patchwork page
#'
#' The margin is applied to the composed page as a whole (via
#' [patchwork::plot_annotation()]) rather than to each individual plot, so
#' the plots in the grid keep their original size instead of shrinking to
#' fit inside the margin.
#' @noRd
add_page_margin <- function(patch, margin = NULL, unit = "in", fill = "white") {
  if (is_null(margin)) {
    return(patch)
  }

  margin <- get_margin(margin, unit = unit)

  patch +
    patchwork::plot_annotation(
      theme = ggplot2::theme(
        plot.margin = margin,
        plot.background = ggplot2::element_rect(
          fill = fill,
          color = NA
        )
      )
    )
}

#' Overlay crop marks in the margin around a composed patchwork page
#'
#' Draws a short tick mark in the margin at each row/column boundary of the
#' plot grid (including the outer edges), so a straight-edge cut across the
#' full sheet can be aligned using the marks on opposite sides. Assumes the
#' grid of plots exactly fills the page after subtracting `margin`.
#' @noRd
add_crop_marks <- function(
  patch,
  ncol,
  nrow,
  page_width,
  page_height,
  margin,
  length = 0.15,
  gap = 0.05,
  color = "black",
  linewidth = 0.25
) {
  margin <- as.numeric(margin)
  content_top <- page_height - margin[1]
  content_right <- page_width - margin[2]
  content_bottom <- margin[3]
  content_left <- margin[4]

  card_width <- (content_right - content_left) / ncol
  card_height <- (content_top - content_bottom) / nrow

  x_breaks <- content_left + (0:ncol) * card_width
  y_breaks <- content_bottom + (0:nrow) * card_height

  marks <- ggplot2::ggplot()

  for (x in x_breaks) {
    if (margin[1] > 0) {
      marks <- marks +
        ggplot2::annotate(
          "segment",
          x = x,
          xend = x,
          y = content_top + gap,
          yend = min(content_top + gap + length, page_height),
          color = color,
          linewidth = linewidth
        )
    }
    if (margin[3] > 0) {
      marks <- marks +
        ggplot2::annotate(
          "segment",
          x = x,
          xend = x,
          y = content_bottom - gap,
          yend = max(content_bottom - gap - length, 0),
          color = color,
          linewidth = linewidth
        )
    }
  }

  for (y in y_breaks) {
    if (margin[4] > 0) {
      marks <- marks +
        ggplot2::annotate(
          "segment",
          x = content_left - gap,
          xend = max(content_left - gap - length, 0),
          y = y,
          yend = y,
          color = color,
          linewidth = linewidth
        )
    }
    if (margin[2] > 0) {
      marks <- marks +
        ggplot2::annotate(
          "segment",
          x = content_right + gap,
          xend = min(content_right + gap + length, page_width),
          y = y,
          yend = y,
          color = color,
          linewidth = linewidth
        )
    }
  }

  marks <- marks +
    ggplot2::coord_fixed(
      xlim = c(0, page_width),
      ylim = c(0, page_height),
      expand = FALSE,
      clip = "off"
    ) +
    ggplot2::theme_void()

  # `inset_element()` aligns to the last panel added to `patch` unless the
  # whole composed grid is first collapsed into a single wrapped element
  patchwork::wrap_elements(patch) +
    patchwork::inset_element(
      marks,
      left = 0,
      bottom = 0,
      right = 1,
      top = 1,
      align_to = "full",
      on_top = TRUE,
      clip = FALSE
    )
}

#' @noRd
set_page_grid <- function(
  plots = NULL,
  page = NULL,
  ncol = NULL,
  nrow = NULL,
  dims = NULL,
  images = FALSE,
  dpi = 120,
  ...,
  call = caller_env()
) {
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
      cli_abort(
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
    x = all(has_name(plot_data, c("xmin", "xmax", "ymin", "ymax"))),
    "{.arg dims} can't be determined from {.arg plot} and must be supplied.",
    .fn = cli::cli_abort,
    call = call
  )

  dims <- c(
    "width" = abs(diff(c(plot_data$xmin, plot_data$xmax))),
    "height" = abs(diff(c(plot_data$ymin, plot_data$ymax)))
  )

  as.numeric(page_dims %/% dims)
}
