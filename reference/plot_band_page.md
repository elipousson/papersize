# Arrange one or more paper bands on a page for printing

`plot_band_page()` creates `n_bands` copies of a
[`plot_band()`](https://elipousson.github.io/papersize/reference/plot_band.md)
band and arranges them, at their true printed size, in a grid on `page`
— a thin wrapper around
[`page_layout()`](https://elipousson.github.io/papersize/reference/page_layout.md)
(the same layout function used for a grid of cards), which handles
sizing the grid to the band's true dimensions, shrinking it to fit if
`n_bands` is fewer than `page` holds, positioning that grid via
`position`, gutter spacing, margins, crop marks, and pagination when
`n_bands` doesn't fit on one page. The result can be saved directly with
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).

## Usage

``` r
plot_band_page(
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
  fold_linewidth = linewidth/2,
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
)
```

## Arguments

- paper:

  Paper, page, or card name, or a data.frame with width and height
  columns (as returned by
  [`get_page_size()`](https://elipousson.github.io/papersize/reference/get_page_size.md)
  or
  [`make_page_size()`](https://elipousson.github.io/papersize/reference/make_page_size.md)),
  passed to
  [`as_page()`](https://elipousson.github.io/papersize/reference/as_page.md).
  Sets the width and height of the stack of cards or paper the band
  wraps around.

- orientation:

  Band orientation, either `"horizontal"` or `"vertical"`.
  `"horizontal"` wraps the band left-to-right, crossing the middle of
  the stack, so the band's panels use the `width` of `paper` and the
  band's own height is set by `band_width`. `"vertical"` wraps the band
  top-to-bottom instead, so the panels use `height` and the band's own
  width is set by `band_width`. Default: `"horizontal"`

- n, pt, x:

  Passed to
  [`as_thickness()`](https://elipousson.github.io/papersize/reference/as_thickness.md)
  to calculate the thickness of the stack. Ignored if `thickness` is
  supplied. `pt` is also used to add a small ease allowance (the caliper
  of a single sheet or card) to the folded side segments of the band,
  since a flat band can't wrap tightly around a stack at its exact
  thickness. Ignored if `thickness` is supplied.

- thickness:

  Optional. A `unit` object or number (in the units of `paper`) with the
  thickness of the stack, overriding `n`, `pt`, and `x`. If supplied, no
  ease allowance is added and `thickness` sets the folded side segments
  of the band exactly. Default: `NULL`

- overlap:

  Distance the band extends past the far edge of the stack, forming a
  segment that can be glued or taped closed. A `unit` object or a number
  (in the units of `paper`). Default: `NULL`, which uses 40% of the
  `width` of `paper` (if `orientation` is `"horizontal"`) or 40% of the
  `height` of `paper` (if `orientation` is `"vertical"`) — i.e. 40% of
  whichever dimension of `paper` the band wraps around.

- band_width:

  Height of the band if `orientation` is `"horizontal"`, or width of the
  band if `orientation` is `"vertical"` — the dimension of the band
  perpendicular to the direction it wraps around the stack. A `unit`
  object or a number (in the units of `paper`). Default: `NULL`, which
  uses 40% of the `height` of `paper` (if `orientation` is
  `"horizontal"`) or 40% of the `width` of `paper` (if `orientation` is
  `"vertical"`).

- fill, color, linewidth:

  Fill, outline color, and outline linewidth for the band, Default:
  `"white"`, `"black"`, and `0.5`

- glue_fill:

  Fill color used to distinguish the overlapping segment of the band set
  by `overlap`, Default: `"grey85"`

- fold_linetype, fold_linewidth:

  Linetype and linewidth for the lines marking where the band should be
  folded, Default: `"dashed"` and `linewidth / 2`

- page:

  Paper name or a data.frame with width and height columns, passed to
  [`page_layout()`](https://elipousson.github.io/papersize/reference/page_layout.md).
  Default: `"letter"`

- page_orientation:

  Page orientation, passed to
  [`page_layout()`](https://elipousson.github.io/papersize/reference/page_layout.md)'s
  `orientation` parameter (kept as a separate parameter here to avoid a
  name clash with the band's own `orientation`). Default: `NULL`, which
  uses `page` as supplied without reorienting it (unlike
  [`page_layout()`](https://elipousson.github.io/papersize/reference/page_layout.md)
  itself, which defaults to `"landscape"`).

- n_bands:

  Number of copies of the band to arrange on `page`. Default: `1`

- ncol, nrow:

  The dimensions of the grid to create. If both are `NULL`, dims will be
  used or dims will be determined based on the plot dimensions.

- byrow:

  Analogous to `byrow` in
  [matrix()](https://rdrr.io/r/base/matrix.html). If `FALSE` the plots
  will be filled in in column-major order

- gutter:

  Optional. Spacing to add between plots in the grid, as a single number
  (used for both row and column spacing), a length-2 numeric vector
  `c(row, col)`, or a named vector or list with `row` and `col`
  elements. Interpreted in `unit`. Implemented by adding half of
  `gutter` to the interior-facing sides of each plot's own `plot.margin`
  (so two adjacent plots each contribute half, summing to the full
  gutter between them) based on its row/column position in the `ncol` x
  `nrow` grid — this replaces each plot's existing `plot.margin`. Plots
  on the outer edge of the grid are not padded on their outward-facing
  side; use `margin` for space around the outside of the whole grid.
  Default: `NULL` (no extra spacing).

  `gutter` changes the total size of the combined grid (adding
  `(ncol - 1) * col_gutter` and `(nrow - 1) * row_gutter`), which
  `marks` accounts for automatically — the automatic `ncol`/`nrow`
  calculation from `page`/`dims` also reserves room for it, so fewer
  plots may fit per page than with `gutter = 0`.

- margin:

  Optional. A margin to add around the outside of the combined grid of
  plots, e.g. so the grid can be centered on a larger sheet of paper
  when saved with
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).
  Passed to
  [`get_margin()`](https://elipousson.github.io/papersize/reference/margins.md)
  with unit. The margin pads the composed page rather than the
  individual plots in `plots`, and does not affect the number of rows
  and columns in the grid. Default: `NULL`, which computes a margin from
  `position` whenever plot dimensions are known (supplied via `dims`, or
  auto-detected from the first plot) — see `position`. Set `margin`
  explicitly to override that (or to add a margin when dimensions aren't
  known, e.g. `ncol`/ `nrow` supplied without `dims`).

  `margin` itself always renders correctly regardless of `page`, because
  it is applied as a fixed absolute-unit margin around whatever canvas
  size
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
  is eventually called with. `marks`, below, is the one that depends on
  `page` being set correctly — see `marks`.

- position:

  Where to place the bands on `page` when `n_bands` doesn't fill it
  completely. Passed to
  [`page_layout()`](https://elipousson.github.io/papersize/reference/page_layout.md)'s
  `position` parameter, but defaults to `"center"` here rather than
  [`page_layout()`](https://elipousson.github.io/papersize/reference/page_layout.md)'s
  own `"top-left"` default.

- unit:

  Unit used for `gutter`, and for `margin` if margin is a bare numeric
  vector or list (ignored for `margin` if it is a `unit` class object;
  `gutter` does not support `unit` class objects). Default: `"in"`.

- marks:

  If `TRUE`, add crop marks in the `margin` area showing where to cut
  the page into individual plots. Requires `margin`. Marks are placed
  assuming the grid of plots exactly fills the page after subtracting
  `margin`, i.e. the same assumption `margin` itself relies on. Default:
  `FALSE`.

  Unlike `margin`, `marks` reads `page` (via
  [`get_page_dims()`](https://elipousson.github.io/papersize/reference/get_page_size.md))
  to work out where the margin area is, so **`page` must equal the exact
  final output size** — the same width/height you pass to
  [`ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html) —
  even if `ncol`/`nrow` are supplied directly and `page` would otherwise
  be unused. Passing just the combined size of the grid of plots (i.e.
  `page` without `margin` added on) will place the marks in the wrong
  location, because
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
  will render a larger canvas than `marks` was calculated for.

- paginate:

  If `TRUE`, create a list of `patchwork` objects when the number of
  plots is greater than the number of spaces in the plot layout. Default
  to `TRUE`.

## Value

A `patchwork` object, or (if `n_bands` doesn't fit on a single `page`
and `paginate = TRUE`) a list of `patchwork` objects — one per page.
Save with
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).

## See also

[`plot_band()`](https://elipousson.github.io/papersize/reference/plot_band.md)

[`page_layout()`](https://elipousson.github.io/papersize/reference/page_layout.md)

## Examples

``` r
if (FALSE) { # \dontrun{
if (interactive() && is_installed(c("ggplot2", "patchwork"))) {
  plot_band_page(get_card("Poker"), n = 54, n_bands = 4)

  plot_band_page(
    get_card("Poker"),
    n = 54,
    n_bands = 1,
    position = "bottom-right"
  )
}
} # }
```
