#' Calculate the thickness of a stack of cards or paper
#'
#' [as_thickness()] estimates the thickness of a stack of `n` sheets of paper
#' or cards, each `pt` in caliper thickness (in points, i.e. thousandths of an
#' inch). Alternatively, supply a known thickness directly with `x`. Used by
#' [plot_band()] to size the folded sides of a band that wraps around a
#' stack.
#'
#' @param n Number of sheets or cards in the stack. Ignored if `x` is
#'   supplied. Either `n` or `x` must be supplied.
#' @param pt Caliper thickness of a single sheet or card, in points (1/1000
#'   in). Ignored if `x` is supplied. Default: 10, a typical thickness for a
#'   playing card.
#' @param x Optional. A known stack thickness (numeric or `unit` object). If
#'   supplied, `n` and `pt` are ignored.
#' @inheritParams as_unit
#' @param ... Not used.
#' @returns A `unit` object with the thickness of the stack.
#' @examples
#' as_thickness(n = 54)
#'
#' as_thickness(x = 0.75, units = "in")
#' @export
#' @importFrom cliExtras cli_abort_if
as_thickness <- function(..., n = NULL, pt = 10, x = NULL, units = NULL) {
  cliExtras::cli_abort_if(
    "{.arg n} or {.arg x} must be supplied to calculate thickness.",
    condition = is_null(n) && is_null(x)
  )

  if (is.null(x)) {
    units <- "in"
    x <- n * (pt / 1000)
  }

  as_unit(x, units)
}

#' Plot a folded paper band to bundle a stack of cards or paper
#'
#' [plot_band()] creates a ggplot2 plot of a flat, foldable paper band sized
#' to wrap around a stack of cards or paper and hold it together, similar to
#' a currency or belly band. `paper` sets the width and height of the item
#' being bundled, `orientation` sets which of those two dimensions the band
#' wraps around, and the folded sides of the band (that wrap around the two
#' edges of the stack) are sized to match the stack thickness calculated by
#' [as_thickness()]. The band extends past the far edge of the stack with an
#' additional segment so it can be glued or taped closed after wrapping.
#'
#' @param paper Paper, page, or card name, or a data.frame with width and
#'   height columns (as returned by [get_page_size()] or [make_page_size()]),
#'   passed to [as_page()]. Sets the width and height of the stack of cards
#'   or paper the band wraps around.
#' @param orientation Band orientation, either `"horizontal"` or
#'   `"vertical"`. `"horizontal"` wraps the band left-to-right, crossing the
#'   middle of the stack, so the band's panels use the `width` of `paper` and
#'   the band's own height is set by `band_width`. `"vertical"` wraps the
#'   band top-to-bottom instead, so the panels use `height` and the band's
#'   own width is set by `band_width`. Default: `"horizontal"`
#' @param n,pt,x Passed to [as_thickness()] to calculate the thickness of the
#'   stack. Ignored if `thickness` is supplied. `pt` is also used to add a
#'   small ease allowance (the caliper of a single sheet or card) to the
#'   folded side segments of the band, since a flat band can't wrap tightly
#'   around a stack at its exact thickness. Ignored if `thickness` is
#'   supplied.
#' @param thickness Optional. A `unit` object or number (in the units of
#'   `paper`) with the thickness of the stack, overriding `n`, `pt`, and `x`.
#'   If supplied, no ease allowance is added and `thickness` sets the folded
#'   side segments of the band exactly. Default: `NULL`
#' @param overlap Distance the band extends past the far edge of the stack,
#'   forming a segment that can be glued or taped closed. A `unit` object or
#'   a number (in the units of `paper`). Default: `NULL`, which uses 40% of
#'   the `width` of `paper` (if `orientation` is `"horizontal"`) or 40% of
#'   the `height` of `paper` (if `orientation` is `"vertical"`) — i.e. 40%
#'   of whichever dimension of `paper` the band wraps around.
#' @param band_width Height of the band if `orientation` is `"horizontal"`,
#'   or width of the band if `orientation` is `"vertical"` — the dimension of
#'   the band perpendicular to the direction it wraps around the stack. A
#'   `unit` object or a number (in the units of `paper`). Default: `NULL`,
#'   which uses 40% of the `height` of `paper` (if `orientation` is
#'   `"horizontal"`) or 40% of the `width` of `paper` (if `orientation` is
#'   `"vertical"`).
#' @param fill,color,linewidth Fill, outline color, and outline linewidth for
#'   the band, Default: `"white"`, `"black"`, and `0.5`
#' @param glue_fill Fill color used to distinguish the overlapping segment of
#'   the band set by `overlap`, Default: `"grey85"`
#' @param fold_linetype,fold_linewidth Linetype and linewidth for the lines
#'   marking where the band should be folded, Default: `"dashed"` and
#'   `linewidth / 2`
#' @return A ggplot2 object.
#' @examples
#' \dontrun{
#' if (interactive() && is_installed("ggplot2")) {
#'   plot_band(get_card("Poker"), n = 54)
#'
#'   plot_band(get_card("Tarot"), n = 78, orientation = "vertical")
#' }
#' }
#' @seealso [as_thickness()]
#' @seealso [plot_band_dims()]
#' @keywords card
#' @export
#' @importFrom rlang arg_match
plot_band <- function(
  paper,
  orientation = c("horizontal", "vertical"),
  n = NULL,
  pt = 10,
  x = NULL,
  thickness = NULL,
  overlap = NULL,
  band_width = NULL,
  fill = "white",
  color = "black",
  linewidth = 0.5,
  glue_fill = "grey85",
  fold_linetype = "dashed",
  fold_linewidth = linewidth / 2
) {
  check_installed("ggplot2")
  orientation <- arg_match(orientation)

  band <- band_layout(
    paper = paper,
    orientation = orientation,
    n = n,
    pt = pt,
    x = x,
    thickness = thickness,
    overlap = overlap,
    band_width = band_width
  )

  plot_dims <- band_to_inches(band)

  segments <- make_band_segments(
    plot_dims$main,
    plot_dims$thickness,
    plot_dims$overlap,
    fill,
    glue_fill
  )

  plot_band_segments(
    segments,
    cross = plot_dims$band_width,
    orientation = orientation,
    color = color,
    linewidth = linewidth,
    fold_linetype = fold_linetype,
    fold_linewidth = fold_linewidth
  )
}

#' Calculate the printed dimensions of a folded paper band
#'
#' [plot_band_dims()] calculates the overall printed width and height of the
#' band [plot_band()] draws for a given stack of cards or paper, including
#' the fold-ease [plot_band()] adds to the stack thickness (see
#' [plot_band()]'s `pt` parameter) and the `overlap` segment. Use it to get
#' the band's actual size — e.g. to center [plot_band()]'s plot on a larger
#' page with [print_to_page()] — without re-deriving the layout by hand.
#'
#' @inheritParams plot_band
#' @returns A data.frame with the band's overall `width`, `height`, and
#'   `units`, as returned by [make_page_size()].
#' @examples
#' plot_band_dims(get_card("Poker"), n = 54)
#'
#' plot_band_dims(get_card("Tarot"), n = 78, orientation = "vertical")
#' @seealso [plot_band()]
#' @keywords card
#' @export
#' @importFrom rlang arg_match
plot_band_dims <- function(
  paper,
  orientation = c("horizontal", "vertical"),
  n = NULL,
  pt = 10,
  x = NULL,
  thickness = NULL,
  overlap = NULL,
  band_width = NULL
) {
  orientation <- arg_match(orientation)

  band <- band_layout(
    paper = paper,
    orientation = orientation,
    n = n,
    pt = pt,
    x = x,
    thickness = thickness,
    overlap = overlap,
    band_width = band_width
  )

  wrap_length <- (2 * band$main) + (2 * band$thickness) + band$overlap

  if (orientation == "horizontal") {
    width <- wrap_length
    height <- band$band_width
  } else {
    width <- band$band_width
    height <- wrap_length
  }

  make_page_size(width = width, height = height, units = band$units)
}

#' Arrange one or more paper bands on a page for printing
#'
#' [plot_band_page()] creates `n_bands` copies of a [plot_band()] band and
#' arranges them, at their true printed size, in a grid on `page` — a thin
#' wrapper around [page_layout()] (the same layout function used for a grid
#' of cards), which handles sizing the grid to the band's true dimensions,
#' shrinking it to fit if `n_bands` is fewer than `page` holds, positioning
#' that grid via `position`, gutter spacing, margins, crop marks, and
#' pagination when `n_bands` doesn't fit on one page. The result can be
#' saved directly with [ggplot2::ggsave()].
#'
#' @inheritParams plot_band
#' @param page Paper name or a data.frame with width and height columns,
#'   passed to [page_layout()]. Default: `"letter"`
#' @param page_orientation Page orientation, passed to [page_layout()]'s
#'   `orientation` parameter (kept as a separate parameter here to avoid a
#'   name clash with the band's own `orientation`). Default: `NULL`, which
#'   uses `page` as supplied without reorienting it (unlike [page_layout()]
#'   itself, which defaults to `"landscape"`).
#' @param n_bands Number of copies of the band to arrange on `page`.
#'   Default: `1`
#' @param position Where to place the bands on `page` when `n_bands` doesn't
#'   fill it completely. Passed to [page_layout()]'s `position` parameter,
#'   but defaults to `"center"` here rather than [page_layout()]'s own
#'   `"top-left"` default.
#' @inheritParams page_layout
#' @return A `patchwork` object, or (if `n_bands` doesn't fit on a single
#'   `page` and `paginate = TRUE`) a list of `patchwork` objects — one per
#'   page. Save with [ggplot2::ggsave()].
#' @examples
#' \dontrun{
#' if (interactive() && is_installed(c("ggplot2", "patchwork"))) {
#'   plot_band_page(get_card("Poker"), n = 54, n_bands = 4)
#'
#'   plot_band_page(
#'     get_card("Poker"),
#'     n = 54,
#'     n_bands = 1,
#'     position = "bottom-right"
#'   )
#' }
#' }
#' @seealso [plot_band()]
#' @seealso [page_layout()]
#' @keywords card
#' @export
#' @importFrom rlang arg_match
plot_band_page <- function(
  paper,
  orientation = c("horizontal", "vertical"),
  n = NULL,
  pt = 10,
  x = NULL,
  thickness = NULL,
  overlap = NULL,
  band_width = NULL,
  fill = "white",
  color = "black",
  linewidth = 0.5,
  glue_fill = "grey85",
  fold_linetype = "dashed",
  fold_linewidth = linewidth / 2,
  page = "letter",
  page_orientation = NULL,
  n_bands = 1,
  ncol = NULL,
  nrow = NULL,
  byrow = FALSE,
  gutter = NULL,
  margin = NULL,
  position = "center",
  unit = "in",
  marks = FALSE,
  paginate = TRUE
) {
  check_installed(c("ggplot2", "patchwork"))
  orientation <- arg_match(orientation)

  band <- plot_band(
    paper = paper,
    orientation = orientation,
    n = n,
    pt = pt,
    x = x,
    thickness = thickness,
    overlap = overlap,
    band_width = band_width,
    fill = fill,
    color = color,
    linewidth = linewidth,
    glue_fill = glue_fill,
    fold_linetype = fold_linetype,
    fold_linewidth = fold_linewidth
  )

  page_layout(
    plots = rep(list(band), n_bands),
    page = page,
    orientation = page_orientation,
    ncol = ncol,
    nrow = nrow,
    byrow = byrow,
    gutter = gutter,
    margin = margin,
    position = position,
    unit = unit,
    marks = marks,
    paginate = paginate
  )
}

#' Calculate the shared layout values used by plot_band() and
#' plot_band_dims()
#'
#' @noRd
#' @importFrom cliExtras cli_abort_if
band_layout <- function(
  paper,
  orientation = "horizontal",
  n = NULL,
  pt = 10,
  x = NULL,
  thickness = NULL,
  overlap = NULL,
  band_width = NULL
) {
  cliExtras::cli_abort_if(
    "{.arg thickness} must be supplied, or {.arg n} or {.arg x} must be
    supplied to calculate it with {.fn as_thickness}.",
    condition = is_null(thickness) && is_null(n) && is_null(x)
  )

  paper <- as_page(paper)
  check_page(paper, cols = c("width", "height"), n = 1)

  units <- paper[[get_units_col()]] %||% "in"

  has_thickness <- !is_null(thickness)
  thickness <- thickness %||% as_thickness(n = n, pt = pt, x = x)
  thickness <- as_band_dist(thickness, units)

  if (!has_thickness) {
    ease <- as_band_dist(as_unit(pt / 1000, "in"), units)
    thickness <- thickness + ease
  }

  if (orientation == "horizontal") {
    main <- paper[["width"]]
    cross <- paper[["height"]]
  } else {
    main <- paper[["height"]]
    cross <- paper[["width"]]
  }

  overlap <- overlap %||% (0.4 * main)
  overlap <- as_band_dist(overlap, units)

  band_width <- band_width %||% (0.4 * cross)
  band_width <- as_band_dist(band_width, units)

  list(
    main = main,
    thickness = thickness,
    overlap = overlap,
    band_width = band_width,
    units = units
  )
}

#' Convert a band_layout() result's dimensions to inches
#'
#' [plot_band()]'s plotted data must always be in inches, regardless of what
#' units `paper` was supplied in, because [page_layout()] (via its internal
#' `set_page_grid()`) auto-detects a plot's true size from its data
#' coordinates with [ggplot2::layer_data()] — plain numbers with no unit
#' metadata attached — and assumes they're inches (the same implicit
#' convention [plot_cards()]'s inches-only card tables already rely on).
#' Without this conversion, a `paper` supplied in any other unit (e.g. "cm")
#' would silently misalign [plot_band_page()]'s auto-computed grid against
#' `page`. [plot_band_dims()] reports the band's size in `paper`'s original
#' units and doesn't need this conversion.
#'
#' @noRd
band_to_inches <- function(band) {
  to_in <- function(x) {
    convert_unit_type(as_unit(x, band[["units"]]), to = "in", valueOnly = TRUE)
  }

  list(
    main = to_in(band[["main"]]),
    thickness = to_in(band[["thickness"]]),
    overlap = to_in(band[["overlap"]]),
    band_width = to_in(band[["band_width"]])
  )
}

#' Convert a band distance (thickness or overlap) to a number in paper units
#'
#' @noRd
as_band_dist <- function(x, units = "in") {
  if (!is_unit(x)) {
    x <- as_unit(x, units)
  }

  convert_unit_type(x, to = units, valueOnly = TRUE)
}

#' Build the data frame of segments for a band, laid out along a single axis
#'
#' Segments run, in order: a main panel (covering the front of the stack), a
#' side flap the width of the stack thickness, a second main panel (covering
#' the back of the stack), a second side flap, and a glue/tape overlap
#' segment that extends beyond the stack's circumference.
#'
#' @noRd
make_band_segments <- function(main, thickness, overlap, fill, glue_fill) {
  part <- c("main", "side", "main", "side", "glue")
  length <- c(main, thickness, main, thickness, overlap)

  end <- cumsum(length)
  start <- end - length

  data.frame(
    part = factor(part, levels = c("main", "side", "glue")),
    start = start,
    end = end,
    fill = c(fill, fill, fill, fill, glue_fill)
  )
}

#' Draw the band segments, fold lines, and outline as a ggplot2 plot
#'
#' @noRd
plot_band_segments <- function(
  band,
  cross,
  orientation = "horizontal",
  color = "black",
  linewidth = 0.5,
  fold_linetype = "dashed",
  fold_linewidth = 0.25
) {
  fold_at <- data.frame(at = band[["end"]][-nrow(band)])
  outline <- data.frame(start = min(band[["start"]]), end = max(band[["end"]]))

  if (orientation == "horizontal") {
    segment_aes <- ggplot2::aes(
      xmin = start,
      xmax = end,
      ymin = 0,
      ymax = cross,
      fill = fill
    )
    outline_aes <- ggplot2::aes(
      xmin = start,
      xmax = end,
      ymin = 0,
      ymax = cross
    )
    fold_aes <- ggplot2::aes(x = at, xend = at, y = 0, yend = cross)
  } else {
    segment_aes <- ggplot2::aes(
      ymin = start,
      ymax = end,
      xmin = 0,
      xmax = cross,
      fill = fill
    )
    outline_aes <- ggplot2::aes(
      ymin = start,
      ymax = end,
      xmin = 0,
      xmax = cross
    )
    fold_aes <- ggplot2::aes(x = 0, xend = cross, y = at, yend = at)
  }

  ggplot2::ggplot(band) +
    ggplot2::geom_rect(segment_aes, color = NA) +
    ggplot2::geom_rect(
      data = outline,
      mapping = outline_aes,
      fill = NA,
      color = color,
      linewidth = linewidth
    ) +
    ggplot2::geom_segment(
      data = fold_at,
      mapping = fold_aes,
      color = color,
      linewidth = fold_linewidth,
      linetype = fold_linetype
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_fixed(expand = FALSE) +
    ggplot2::theme_void()
}
