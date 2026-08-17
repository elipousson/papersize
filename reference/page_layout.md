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
  width and height are both provided, Default: `NULL`

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

- unit:

  Unit used for `margin` if margin is a bare numeric vector or list.
  Ignored if margin is a `unit` class object. Default: `"in"`.

- marks:

  If `TRUE`, add crop marks in the `margin` area showing where to cut
  the page into individual plots. Requires `margin`. Marks are placed
  assuming the grid of plots exactly fills the page after subtracting
  `margin`, i.e. the same assumption `margin` itself relies on. Default:
  `FALSE`.

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

page_layout(
  plots = plot_cards("Poker", 6),
  page = make_page_size(width = 2.5 * 3, height = 3.5 * 2, units = "in"),
  ncol = 3,
  nrow = 2,
  margin = margins(t = 0.75, r = 0.5, b = 0.75, l = 0.5, unit = "in"),
  marks = TRUE
)
#> Warning: `orientation` can't be set to "landscape" when the page width is 7.5 and height
#> is 7.
#> ℹ Orientation kept as "square".
#> $`1`

#> 
```
