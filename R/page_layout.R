#' Use patchwork to lay out a list of fixed aspect plots on a larger page
#'
#' @param plots Page name, a data.frame with width and height columns, or a list
#'   of ggplot2 objects with card plots. Default: `NULL`
#' @param page Paper name or a data.frame with width and height columns.
#'   Optional if width and height are both provided, Default: `NULL`. If
#'   `ncol` and `nrow` are also supplied, `page` is not used to determine the
#'   grid size — but if `margin` or `marks` are used, `page` must still be
#'   set to the exact final output size (the same width/height you plan to
#'   pass to [ggplot2::ggsave()]), not just the combined size of the grid of
#'   plots. See `margin` and `marks` for why this matters.
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
#' @param gutter Optional. Spacing to add between plots in the grid, as a
#'   single number (used for both row and column spacing), a length-2 numeric
#'   vector `c(row, col)`, or a named vector or list with `row` and `col`
#'   elements. Interpreted in `unit`. Implemented by adding half of `gutter`
#'   to the interior-facing sides of each plot's own `plot.margin` (so two
#'   adjacent plots each contribute half, summing to the full gutter between
#'   them) based on its row/column position in the `ncol` x `nrow` grid —
#'   this replaces each plot's existing `plot.margin`. Plots on the outer
#'   edge of the grid are not padded on their outward-facing side; use
#'   `margin` for space around the outside of the whole grid. Default: `NULL`
#'   (no extra spacing).
#'
#'   `gutter` changes the total size of the combined grid (adding
#'   `(ncol - 1) * col_gutter` and `(nrow - 1) * row_gutter`), which `marks`
#'   accounts for automatically — but `set_page_grid()`'s automatic `ncol`/
#'   `nrow` calculation from `page`/`dims` does not reserve extra room for
#'   `gutter`, so pass `ncol`/`nrow` explicitly when combining `gutter` with
#'   an auto-computed grid size.
#' @param margin Optional. A margin to add around the outside of the combined
#'   grid of plots, e.g. so the grid can be centered on a larger sheet of
#'   paper when saved with [ggplot2::ggsave()]. Passed to [get_margin()] with
#'   unit. The margin pads the composed page rather than the individual plots
#'   in `plots`, and does not affect the number of rows and columns in the
#'   grid. Default: `NULL`.
#'
#'   `margin` itself always renders correctly regardless of `page`, because
#'   it is applied as a fixed absolute-unit margin around whatever canvas
#'   size [ggplot2::ggsave()] is eventually called with. `marks`, below, is
#'   the one that depends on `page` being set correctly — see `marks`.
#' @param unit Unit used for `gutter`, and for `margin` if margin is a bare
#'   numeric vector or list (ignored for `margin` if it is a `unit` class
#'   object; `gutter` does not support `unit` class objects). Default:
#'   `"in"`.
#' @param marks If `TRUE`, add crop marks in the `margin` area showing where
#'   to cut the page into individual plots. Requires `margin`. Marks are
#'   placed assuming the grid of plots exactly fills the page after
#'   subtracting `margin`, i.e. the same assumption `margin` itself relies
#'   on. Default: `FALSE`.
#'
#'   Unlike `margin`, `marks` reads `page` (via [get_page_dims()]) to work
#'   out where the margin area is, so **`page` must equal the exact final
#'   output size** — the same width/height you pass to `ggsave()` — even if
#'   `ncol`/`nrow` are supplied directly and `page` would otherwise be
#'   unused. Passing just the combined size of the grid of plots (i.e.
#'   `page` without `margin` added on) will place the marks in the wrong
#'   location, because [ggplot2::ggsave()] will render a larger canvas than
#'   `marks` was calculated for.
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
#' # `page` must be the final output size (grid of 3x2 Poker cards, 7.5x7in,
#' # plus the 0.5/0.75in margin on each side = 8.5x8.5in), not just the size
#' # of the grid of plots — this is what ggsave(width, height) should match
#' page_layout(
#'   plots = plot_cards("Poker", 6),
#'   page = make_page_size(width = 8.5, height = 8.5, units = "in"),
#'   ncol = 3,
#'   nrow = 2,
#'   margin = margins(t = 0.75, r = 0.5, b = 0.75, l = 0.5, unit = "in"),
#'   marks = TRUE
#' )
#'
#' # `gutter` adds spacing between plots, which grows the grid (3x2 Poker
#' # cards with a 0.1in gutter = 7.7x7.1in) — `page` (and `marks`) account
#' # for it automatically, so it still needs the full 8.7x8.6in page size
#' page_layout(
#'   plots = plot_cards("Poker", 6),
#'   page = make_page_size(width = 8.7, height = 8.6, units = "in"),
#'   ncol = 3,
#'   nrow = 2,
#'   gutter = 0.1,
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
  gutter = NULL,
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

  gutter_dims <- get_gutter(gutter, unit = unit, call = call)

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
    plots <- add_gutter_margins(
      plots,
      ncol = page_grid[[1]],
      nrow = page_grid[[2]],
      byrow = byrow,
      gutter = gutter_dims,
      unit = unit
    )

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
        margin = get_margin(margin, unit = unit),
        gutter = gutter_dims
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
      x <- add_gutter_margins(
        x,
        ncol = page_grid[[1]],
        nrow = page_grid[[2]],
        byrow = byrow,
        gutter = gutter_dims,
        unit = unit
      )

      patch_layout <- patchwork::wrap_plots(
        x,
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
          margin = get_margin(margin, unit = unit),
          gutter = gutter_dims
        )
      }

      patch_layout
    }
  )
}

# TODO: Determine if unused unit argument for get_gutter is required
#' Parse the `gutter` argument into a `c(row =, col =)` numeric pair
#' @noRd
get_gutter <- function(gutter = NULL, unit = "in", call = caller_env()) {
  if (is_null(gutter)) {
    return(c(row = 0, col = 0))
  }

  if (is_list(gutter)) {
    gutter <- unlist(gutter)
  }

  if (all(has_name(gutter, c("row", "col")))) {
    return(c(
      row = as.numeric(gutter[["row"]]),
      col = as.numeric(gutter[["col"]])
    ))
  }

  if (has_length(gutter, 1)) {
    gutter <- as.numeric(gutter)
    return(c(row = gutter, col = gutter))
  }

  if (has_length(gutter, 2)) {
    gutter <- as.numeric(gutter)
    return(c(row = gutter[[1]], col = gutter[[2]]))
  }

  cli_abort(
    "{.arg gutter} must be a single number, a length-2 numeric vector, or a
    named vector or list with {.val row} and {.val col} elements.",
    call = call
  )
}

#' Add interior spacing between plots by patching each plot's own margin
#'
#' Adds half of `gutter` to the grid-interior-facing sides of each plot's
#' `plot.margin`, based on its row/column position in the `ncol` x `nrow`
#' grid (matching `patchwork::wrap_plots()`'s own `byrow` fill order), so two
#' adjacent plots each contribute half the gutter and it sums to the full
#' amount between them. Plots on the outer edge of the grid are left at 0 on
#' their outward-facing side — pair with `add_page_margin()` for space
#' around the outside of the whole grid. This replaces each plot's existing
#' `plot.margin` entirely.
#' @noRd
add_gutter_margins <- function(
  plots,
  ncol,
  nrow,
  byrow = FALSE,
  gutter = c(row = 0, col = 0),
  unit = "in"
) {
  if (all(gutter == 0)) {
    return(plots)
  }

  pos <- matrix(seq_len(ncol * nrow), nrow = nrow, ncol = ncol, byrow = byrow)

  map(
    seq_along(plots),
    function(k) {
      rc <- which(pos == k, arr.ind = TRUE)
      row <- rc[1, "row"]
      col <- rc[1, "col"]

      plots[[k]] +
        ggplot2::theme(
          plot.margin = ggplot2::margin(
            t = if (row > 1) gutter[["row"]] / 2 else 0,
            r = if (col < ncol) gutter[["col"]] / 2 else 0,
            b = if (row < nrow) gutter[["row"]] / 2 else 0,
            l = if (col > 1) gutter[["col"]] / 2 else 0,
            unit = unit
          )
        )
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
  gutter = c(row = 0, col = 0),
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

  card_width <- (content_right - content_left - (ncol - 1) * gutter[["col"]]) /
    ncol
  card_height <- (content_top - content_bottom - (nrow - 1) * gutter[["row"]]) /
    nrow

  # each card's own left/right (or bottom/top) edges, deduped — collapses to
  # the same evenly-spaced breaks as before when gutter is 0, since adjacent
  # cards' edges then coincide
  col_left <- content_left + (0:(ncol - 1)) * (card_width + gutter[["col"]])
  x_breaks <- sort(unique(c(col_left, col_left + card_width)))

  row_bottom <- content_bottom +
    (0:(nrow - 1)) * (card_height + gutter[["row"]])
  y_breaks <- sort(unique(c(row_bottom, row_bottom + card_height)))

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

  # `wrap_elements()` and the composition created by adding `inset_element()`
  # each pick up ggplot2's default ~5.5pt `plot.margin`/background on top of
  # whatever `patch` already has, shrinking the content area on every side
  # (visible as marks landing inside `margin` rather than at its edge) unless
  # both are explicitly zeroed out here
  no_margin <- ggplot2::theme(
    plot.margin = ggplot2::margin(0, 0, 0, 0),
    plot.background = ggplot2::element_rect(fill = NA, color = NA)
  )

  # `inset_element()` aligns to the last panel added to `patch` unless the
  # whole composed grid is first collapsed into a single wrapped element
  (patchwork::wrap_elements(patch) + no_margin) +
    patchwork::inset_element(
      marks,
      left = 0,
      bottom = 0,
      right = 1,
      top = 1,
      align_to = "full",
      on_top = TRUE,
      clip = FALSE
    ) &
    no_margin
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
