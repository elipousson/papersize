# Explicitly draw plot using dimensions from page data.frame or list

**\[experimental\]**

## Usage

``` r
print_to_page(plot, page, newpage = TRUE, vp = NULL, ...)

print_to_page_layout(
  plot,
  page,
  row_position = 1,
  col_position = 1,
  row_height = NULL,
  col_width = NULL,
  nrow = 1,
  ncol = 1,
  layout = NULL,
  parent = NULL,
  children = NULL,
  filename = NULL,
  ...
)
```

## Arguments

- plot:

  Plot to display

- page:

  A page data.frame from
  [`get_page_size()`](https://elipousson.github.io/papersize/reference/get_page_size.md)
  or
  [`make_page_size()`](https://elipousson.github.io/papersize/reference/make_page_size.md).

- newpage:

  draw new (empty) page first?

- vp:

  viewport to draw plot in

- ...:

  Arguments passed on to
  [`page_to_viewport`](https://elipousson.github.io/papersize/reference/page_to_viewport.md),
  [`page_to_layout`](https://elipousson.github.io/papersize/reference/page_to_layout.md)

  `name`

  :   A character value to uniquely identify the viewport once it has
      been pushed onto the viewport tree.

  `cols`

  :   Column names to use for width and height columns. Defaults to
      c("width", "height"). Must be length 2 and the first value is
      always used as as the width name and the second as the height.

  `margins`

  :   A numeric list or vector or a margin class object. Defaults to
      `NULL`. margins are removed from the overall layout width and
      height.

  `region`

  :   Optional. An additional page data.frame where the region width and
      height are used as the column width and row height.

  `gutter`

  :   Gutter width/height. Not yet implemented.

  `units`

  :   Passed to default.units parameter of
      [`grid::grid.layout()`](https://rdrr.io/r/grid/grid.layout.html).

  `widths`

  :   A numeric vector or unit object describing the widths of the
      columns in the layout.

  `heights`

  :   A numeric vector or unit object describing the heights of the rows
      in the layout.

  `respect`

  :   A logical value or a numeric matrix. If a logical, this indicates
      whether row heights and column widths should respect each other.
      If a matrix, non-zero values indicate that the corresponding row
      and column should be respected (see examples below).

  `just`

  :   A string or numeric vector specifying how the layout should be
      justified if it is not the same size as its parent viewport. If
      there are two values, the first value specifies horizontal
      justification and the second value specifies vertical
      justification. Possible string values are: `"left"`, `"right"`,
      `"centre"`, `"center"`, `"bottom"`, and `"top"`. For numeric
      values, 0 means left alignment and 1 means right alignment. NOTE
      that in this context, `"left"`, for example, means align the left
      edge of the left-most layout column with the left edge of the
      parent viewport.

- row_position, col_position:

  Row and column position. If nrow is smaller than row_position or ncol
  is smaller than col_position, row_position is used for nrow or
  col_position is used for ncol instead.

- row_height, col_width:

  Row height and column width.

- nrow:

  An integer describing the number of rows in the layout.

- ncol:

  An integer describing the number of columns in the layout.

- layout:

  Passed to
  [`page_to_viewport()`](https://elipousson.github.io/papersize/reference/page_to_viewport.md)
  with layout_position as layout.pos.row and layout.pos.col if provided.
  Defaults to `NULL` where layout is defined by page_to_layout using
  ncol, nrow, page and any additional parameters passed to `...`

- parent:

  A grid viewport object.

- children:

  A vpList object.

- filename:

  File name to create on disk.

## Value

Invisibly returns the original plot using a viewport from
[`page_to_viewport()`](https://elipousson.github.io/papersize/reference/page_to_viewport.md).

## See also

[`ggplot2::print.ggplot()`](https://ggplot2.tidyverse.org/reference/print.ggplot.html)

## Examples

``` r
if (FALSE) { # \dontrun{
if (interactive() && is_installed("ggplot2")) {
  library(ggplot2)

  p <- ggplot(mpg, aes(displ, hwy, colour = class)) +
    geom_point()

  print_to_page(
    p,
    page = get_page_size("Tarot card")
  )
}
} # }
```
