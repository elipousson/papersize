# Use patchwork to lay out a list of fixed aspect plots on a larger page

Use patchwork to lay out a list of fixed aspect plots on a larger page

## Usage

``` r
page_layout(
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
)
```

## Arguments

- plots:

  Page name, a data.frame with width and height columns, or a list of
  ggplot2 objects with card plots. Default: `NULL`

- page:

  Paper name or a data.frame with width and height columns. Optional if
  width and height are both provided, Default: `NULL`. If `ncol` and
  `nrow` are also supplied, `page` is not used to determine the grid
  size — but if `margin` or `marks` are used, `page` must still be set
  to the exact final output size (the same width/height you plan to pass
  to
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)),
  not just the combined size of the grid of plots. See `margin` and
  `marks` for why this matters.

- width, height:

  Paper width and height, Default: `NULL`

- orientation:

  Paper orientation, Optional if width and height are both provided,
  Default: 'landscape'

- byrow:

  Analogous to `byrow` in
  [matrix()](https://rdrr.io/r/base/matrix.html). If `FALSE` the plots
  will be filled in in column-major order

- guides:

  A string specifying how guides should be treated in the layout.
  `'collect'` will collect guides below to the given nesting level,
  removing duplicates. `'keep'` will stop collection at this level and
  let guides be placed alongside their plot. `auto` will allow guides to
  be collected if a upper level tries, but place them alongside the plot
  if not. If you modify default guide "position" with
  [theme(legend.position=...)](https://ggplot2.tidyverse.org/reference/theme.html)
  while also collecting guides you must apply that change to the overall
  patchwork (see example).

- tag_level:

  A string (`'keep'` or `'new'`) to indicate how auto-tagging should
  behave. See
  [`plot_annotation()`](https://patchwork.data-imaginist.com/reference/plot_annotation.html).

- design:

  Specification of the location of areas in the layout. Can either be
  specified as a text string or by concatenating calls to
  [`area()`](https://patchwork.data-imaginist.com/reference/area.html)
  together. See the examples for further information on use.

- paginate:

  If `TRUE`, create a list of `patchwork` objects when the number of
  plots is greater than the number of spaces in the plot layout. Default
  to `TRUE`.

- ncol, nrow:

  The dimensions of the grid to create. If both are `NULL`, dims will be
  used or dims will be determined based on the plot dimensions.

- dims:

  Optional. Plot dimensions. Ignored if ncol and nrow are supplied.
  Otherwise, if `NULL` (default), dims are inferred based on the
  dimensions of the first plot in plots.

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
  `marks` accounts for automatically — but `set_page_grid()`'s automatic
  `ncol`/ `nrow` calculation from `page`/`dims` does not reserve extra
  room for `gutter`, so pass `ncol`/`nrow` explicitly when combining
  `gutter` with an auto-computed grid size.

- margin:

  Optional. A margin to add around the outside of the combined grid of
  plots, e.g. so the grid can be centered on a larger sheet of paper
  when saved with
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).
  Passed to
  [`get_margin()`](https://elipousson.github.io/papersize/reference/margins.md)
  with unit. The margin pads the composed page rather than the
  individual plots in `plots`, and does not affect the number of rows
  and columns in the grid. Default: `NULL`.

  `margin` itself always renders correctly regardless of `page`, because
  it is applied as a fixed absolute-unit margin around whatever canvas
  size
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
  is eventually called with. `marks`, below, is the one that depends on
  `page` being set correctly — see `marks`.

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

- images:

  Not yet implemented. If `TRUE` and dims is `NULL`, the input plots are
  assumed to be plots created with
  [`magick::image_ggplot()`](https://docs.ropensci.org/magick/reference/image_ggplot.html)
  and dpi is used to infer dimensions.

- dpi:

  Not yet implemented. Resolution.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

A `patchwork` object or a list of `patchwork` objects.

## See also

[`ggplot2::ggplot_build()`](https://ggplot2.tidyverse.org/reference/ggplot_build.html)
[`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html),
[`patchwork::plot_layout()`](https://patchwork.data-imaginist.com/reference/plot_layout.html)

## Examples

``` r
page_layout(
  plots = plot_cards("Poker", 6),
  page = "letter"
)
#> ℹ Using `dims` from first plot in `plots`.
#> $`1`

#> 

# `page` must be the final output size (grid of 3x2 Poker cards, 7.5x7in,
# plus the 0.5/0.75in margin on each side = 8.5x8.5in), not just the size
# of the grid of plots — this is what ggsave(width, height) should match
page_layout(
  plots = plot_cards("Poker", 6),
  page = make_page_size(width = 8.5, height = 8.5, units = "in"),
  ncol = 3,
  nrow = 2,
  margin = margins(t = 0.75, r = 0.5, b = 0.75, l = 0.5, unit = "in"),
  marks = TRUE
)
#> Warning: `orientation` can't be set to "landscape" when the page width is 8.5 and height
#> is 8.5.
#> ℹ Orientation kept as "square".
#> $`1`

#> 

# `gutter` adds spacing between plots, which grows the grid (3x2 Poker
# cards with a 0.1in gutter = 7.7x7.1in) — `page` (and `marks`) account
# for it automatically, so it still needs the full 8.7x8.6in page size
page_layout(
  plots = plot_cards("Poker", 6),
  page = make_page_size(width = 8.7, height = 8.6, units = "in"),
  ncol = 3,
  nrow = 2,
  gutter = 0.1,
  margin = margins(t = 0.75, r = 0.5, b = 0.75, l = 0.5, unit = "in"),
  marks = TRUE
)
#> Warning: `orientation` can't be set to "landscape" when the page width is 8.7 and height
#> is 8.6.
#> ℹ Orientation kept as "square".
#> $`1`

#> 
```
