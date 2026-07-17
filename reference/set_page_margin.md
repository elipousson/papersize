# Set margins for page data.frame (adding body width, height, and asp)

Set margins for page data.frame (adding body width, height, and asp)

## Usage

``` r
set_page_margin(
  page = NULL,
  margins,
  unit = "in",
  cols = c("width", "height"),
  ...
)
```

## Arguments

- page:

  A character vector with a page size name or a data.frame. Passed to x
  parameter of
  [`as_page()`](https://elipousson.github.io/papersize/reference/as_page.md).

- margins:

  Passed to
  [`get_margin()`](https://elipousson.github.io/papersize/reference/margins.md)
  with unit value.

- unit:

  Unit used for the margin. If margin is a unit object, unit is ignored.
  If page uses different units, the margins are converted into the page
  units for consistency.

- cols:

  Column names to use for width and height columns. Defaults to
  c("width", "height"). Must be length 2 and the first value is always
  used as as the width name and the second as the height.

- ...:

  Passed to
  [`as_page()`](https://elipousson.github.io/papersize/reference/as_page.md)
  with page and cols.

## Value

A data.frame with the page dimensions and additional columns for body
dimensions, body aspect ratio, and margins.

## See also

[`ggplot2::margin()`](https://ggplot2.tidyverse.org/reference/element.html);
[`set_page_dims()`](https://elipousson.github.io/papersize/reference/set_page_dims.md);
[`set_page_orientation()`](https://elipousson.github.io/papersize/reference/set_page_dims.md);
[`set_page_asp()`](https://elipousson.github.io/papersize/reference/set_page_dims.md)
