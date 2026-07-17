# Convert character string, page name, or page data.frame to numeric aspect ratio

Convert a character vector with an aspect ratio to a numeric aspect
ratio or get the aspect ratio for a given page that has an aspect ratio
column.

## Usage

``` r
as_asp(
  asp = NULL,
  page = NULL,
  orientation = NULL,
  flipped = FALSE,
  sep = ":",
  cols = c("width", "height"),
  ...
)
```

## Arguments

- asp:

  Aspect ratio of width to height as a numeric value (e.g. 0.33) or
  characters separated by a colon (e.g. "1:3"). If numeric, `as_asp()`
  returns the value of asp without modification.

- page:

  If character, page is passed as the name parameter to
  [`get_page_size()`](https://elipousson.github.io/papersize/reference/get_page_size.md).

- orientation:

  Page orientation, Default: `NULL`. Supported options are "portrait",
  "landscape", or "square".

- flipped:

  If `TRUE`, return aspect ratio of height to width (y / x), instead of
  width to height.

- sep:

  Separator character to use if asp is a character vector. Defaults to
  ":".

- cols:

  Length 2 character vector with column names for page dimensions.
  Defaults to c("width", "height").

- ...:

  Arguments passed on to
  [`get_page_size`](https://elipousson.github.io/papersize/reference/get_page_size.md)

  `name`

  :   Page name, e.g. "letter", not case sensitive, Default: `NULL`

  `width`

  :   Page width in "in", "px" or "mm" units. Default: `NULL`

  `height`

  :   Page height in "in", "px" or "mm" units. Default: `NULL`

  `reorient`

  :   If `TRUE` and orientation is not `NULL`, flip width and height
      dimensions for any pages that do not match the provided
      orientation. Set `reorient = FALSE` to filter pages by
      orientation.

  `type`

  :   Page type, Options include "paper", "social", "postcard", "print",
      "card", or "screen". Default: `NULL`

  `ignore.case`

  :   If `FALSE`, filtering for page and type are case sensitive.
      Defaults to `TRUE`.

  `units`

  :   Units to convert page dimensions to using
      [`convert_unit_type()`](https://elipousson.github.io/papersize/reference/as_unit.md).

## Value

A numeric vector.

## See also

`isstatic::as_orientation()`

## Examples

``` r
as_asp(8.5 / 11)
#> [1] 0.7727273

as_asp("11:17")
#> [1] 0.6470588

as_asp("3/2", sep = "/")
#> [1] 1.5

as_asp(page = "letter")
#> [1] 0.7727273

as_asp(page = "letter", orientation = "landscape")
#> [1] 1.294118

as_asp(page = "letter", orientation = "portrait", flipped = TRUE)
#> [1] 1.294118

as_asp(page = make_page_size(8.5, 11, "in"), flipped = TRUE)
#> [1] 1.294118
```
