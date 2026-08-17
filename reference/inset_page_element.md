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

  `arg`

  :   Passed to
      [`cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html) to
      improve internal error messages.

  `valid_units`

  :   Character vector with name or symbols for valid units. Defaults to
      `NULL` but any other unit name or symbol, e.g. "px", is permitted.

  `from`

  :   Unit to convert from. If `NULL` and x is not a units object,
      convert to `to` units with a warning.

  `to`

  :   Unit to convert to. Passed to unitTo parameter of
      [`grid::convertUnit()`](https://rdrr.io/r/grid/grid.convert.html).
      If `NULL`, return x as is.

  `typeFrom`

  :   Passed to typeFrom parameter of
      [`grid::convertUnit()`](https://rdrr.io/r/grid/grid.convert.html).
      Defaults to "dimension".

  `valueOnly`

  :   Passed to valueOnly parameter of
      [`grid::convertUnit()`](https://rdrr.io/r/grid/grid.convert.html).
      Defaults to `FALSE`.

  `y`

  :   Object to compare to x.

  `x`

  :   A numeric vector.

      For `is.unit`, any R object.

  `units`

  :   A character vector specifying the units for the corresponding
      numeric values.

  `data`

  :   This argument is used to supply extra information for special
      `unit` types.

  `call`

  :   The execution environment of a currently running function, e.g.
      `call = caller_env()`. The corresponding function call is
      retrieved and mentioned in error messages as the source of the
      error.

      You only need to supply `call` when throwing a condition from a
      helper function which wouldn't be relevant to mention in the
      message.

      Can also be `NULL` or a [defused function
      call](https://rlang.r-lib.org/reference/topic-defuse.html) to
      respectively not display any call or hard-code a code to display.

      For more information about error calls, see [Including function
      calls in error
      messages](https://rlang.r-lib.org/reference/topic-error-call.html).

  `recurse`

  :   Whether to recurse into complex units.

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
