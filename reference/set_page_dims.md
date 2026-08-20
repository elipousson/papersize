# Set page data.frame dimensions, orientation, or aspect ratio

Set page data.frame dimensions, orientation, or aspect ratio

set_page_asp: appends an aspect ratio column a page size data.frame

## Usage

``` r
set_page_dims(
  page,
  dims = NULL,
  width = NULL,
  height = NULL,
  units = NULL,
  cols = c("width", "height")
)

set_page_orientation(
  page,
  orientation = NULL,
  tolerance = 0.1,
  cols = c("width", "height")
)

set_page_asp(page, flipped = FALSE, cols = c("width", "height"))
```

## Arguments

- page:

  If character, page is passed as the name parameter to
  [`get_page_size()`](https://elipousson.github.io/papersize/reference/get_page_size.md).

- dims:

  A vector or list with values in the same order as cols (defaults to
  `c(<width>, <height>)` to match cols).

- width, height:

  Page width and height. Both are required, if asp is `NULL`. Default to
  `NULL`.

- units:

  Units for width and height. Required unless units is included in dims
  or `require_units = FALSE`. Passed to
  [`as_unit_type()`](https://elipousson.github.io/papersize/reference/as_unit.md)
  to validate.

- cols:

  Column names to use for width and height columns. Defaults to
  c("width", "height"). Must be length 2 and the first value is always
  used as as the width name and the second as the height.

- orientation:

  Page orientation, Default: `NULL`. Supported options are "portrait",
  "landscape", or "square". If width and height suggest a portrait
  orientation when orientation = "landscape", the dimensions are
  reversed so the page dimensions match the provided orientation.

- tolerance:

  Positive numeric value above or below 1 used to determine if an aspect
  ratio is square, landscape, or portrait.

- flipped:

  If `TRUE`, return aspect ratio of height to width (y / x), instead of
  width to height.
