# Create an inset with page size dimensions be added on top of the previous plot

Create an inset with page size dimensions be added on top of the
previous plot

## Usage

``` r
inset_page_element(
  p,
  inset_page = NULL,
  left = NULL,
  bottom = NULL,
  right = NULL,
  top = NULL,
  align_to = "panel",
  on_top = TRUE,
  clip = TRUE,
  ignore_tag = FALSE,
  ...
)
```

## Arguments

- p:

  A grob, ggplot, patchwork, formula, raster, nativeRaster, or gt object
  to add as an inset

- inset_page:

  Page size data.frame to use for inset, Default: `NULL`

- left, bottom, right, top:

  numerics or units giving the location of the outer bounds. If given as
  numerics and inset_page is NULL, they will be converted to npc units.
  All four are required if inset_page is` NULL`. If inset_page is
  provided, top *or* bottom and left *or* right must be provided as the
  inset element is expected to be the width and height defined by
  inset_page.

- align_to:

  Specifies what `left`, `bottom`, etc should be relative to. Either
  `'panel'` (default), `'plot'`, or `'full'`.

- on_top:

  Logical. Should the inset be placed on top of the other plot or below
  (but above the background)?

- clip:

  Logical. Should clipping be performed on the inset?

- ignore_tag:

  Logical. Should autotagging ignore the inset?

- ...:

  Arguments passed on to
  [`convert_unit_type`](https://elipousson.github.io/papersize/reference/as_unit.md)

  `from`

  :   Unit to convert from. If `NULL` and x is not a units object,
      convert to `to` units with a warning.

  `to`

  :   Unit to convert to. Passed to unitTo parameter of
      [`grid::convertUnit()`](https://rdrr.io/r/grid/grid-defunct.html).
      If `NULL`, return x as is.

  `typeFrom`

  :   Passed to typeFrom parameter of
      [`grid::convertUnit()`](https://rdrr.io/r/grid/grid-defunct.html).
      Defaults to "dimension".

  `valueOnly`

  :   Passed to valueOnly parameter of
      [`grid::convertUnit()`](https://rdrr.io/r/grid/grid-defunct.html).
      Defaults to `FALSE`.

  `x`

  :   A numeric vector.

      For `is.unit`, any R object.

## Value

A `inset_path` object

## See also

[`patchwork::inset_element()`](https://patchwork.data-imaginist.com/reference/inset_element.html)

[`inset_page()`](https://elipousson.github.io/papersize/reference/inset_page.md)

## Examples

``` r
if (FALSE) { # \dontrun{
if (interactive() && is_installed("ggplot2")) {
  library(ggplot2)
  p <- ggplot(mpg, aes(displ, fill = class)) +
    geom_bar()

  ggplot(mpg, aes(displ, hwy, colour = class)) +
    geom_point() +
    inset_page_element(
      p = p,
      inset_page = get_page_size("Poker card", orientation = "landscape"),
      left = unit(1, "in"),
      bottom = unit(1, "in")
    )
}
} # }
```
