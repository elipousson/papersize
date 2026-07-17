# Make a page size data frame

Width and height or aspect ratio and either width or height are
required. Units are also required. If orientation is provided, width and
height may be reversed to match the provided orientation.

## Usage

``` r
make_page_size(
  width = NULL,
  height = NULL,
  units,
  asp = NULL,
  orientation = NULL,
  name = NULL,
  dims = NULL,
  valid_units = NULL,
  cols = c("width", "height"),
  class = "data.frame",
  call = caller_env()
)
```

## Arguments

- width, height:

  Page width and height. Both are required, if asp is `NULL`. Default to
  `NULL`.

- units:

  Units for width and height. Required unless units is included in dims.
  Passed to
  [`as_unit_type()`](https://elipousson.github.io/papersize/reference/as_unit.md)
  to validate.

- asp:

  Aspect ratio. Required if only one of width or height are provided.

- orientation:

  Page orientation, Default: `NULL`. Supported options are "portrait",
  "landscape", or "square". If width and height suggest a portrait
  orientation when orientation = "landscape", the dimensions are
  reversed so the page dimensions match the provided orientation.

- name:

  Optional name for paper size. Recommend avoiding duplication with
  existing names in `paper_sizes`.

- dims:

  A list or data.frame that can be used to set width, height, units,
  and/or asp.

- valid_units:

  Character vector with name or symbols for valid units. Defaults to
  `NULL` but any other unit name or symbol, e.g. "px", is permitted.

- cols:

  Column names to use for width and height columns. Defaults to
  c("width", "height"). Must be length 2 and the first value is always
  used as as the width name and the second as the height.

- class:

  Class of return object: "data.frame" (default) or "list" (only
  supported when input page size is a single row).

- call:

  The execution environment of a currently running function, e.g.
  `call = caller_env()`. The corresponding function call is retrieved
  and mentioned in error messages as the source of the error.

  You only need to supply `call` when throwing a condition from a helper
  function which wouldn't be relevant to mention in the message.

  Can also be `NULL` or a [defused function
  call](https://rlang.r-lib.org/reference/topic-defuse.html) to
  respectively not display any call or hard-code a code to display.

  For more information about error calls, see [Including function calls
  in error
  messages](https://rlang.r-lib.org/reference/topic-error-call.html).

## Value

A data.frame with columns named (by default) width, height, units,
orientation, and asp or a list with those same names.

## See also

[`get_page_size()`](https://elipousson.github.io/papersize/reference/get_page_size.md)

## Examples

``` r
make_page_size(48, 24, "in", name = "Tabletop map")
#>           name width height  units orientation asp
#> 1 Tabletop map    48     24 inches   landscape   2

make_page_size(8.5, 8.5, "in", name = "Trimmed letter")
#>             name width height  units orientation asp
#> 1 Trimmed letter   8.5    8.5 inches      square   1

make_page_size(5, asp = 1.25, units = "cm", class = "list")
#> $width
#> [1] 5
#> 
#> $height
#> [1] 4
#> 
#> $units
#> [1] "cm"
#> 
#> $orientation
#> [1] "landscape"
#> 
#> $asp
#> [1] 1.25
#> 
```
