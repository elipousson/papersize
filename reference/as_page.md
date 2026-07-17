# Coerce a character or named vector to a page data.frame using get_page_size() or make_page_size()

Coerce a character or named vector to a page data.frame using
get_page_size() or make_page_size()

## Usage

``` r
as_page(x, ..., cols = c("width", "height"), class = "data.frame")
```

## Arguments

- x:

  A character vector passed to the name parameter of
  [`get_page_size()`](https://elipousson.github.io/papersize/reference/get_page_size.md),
  a named list of vector passed to dims for
  [`make_page_size()`](https://elipousson.github.io/papersize/reference/make_page_size.md),
  or a length 2 numeric vector passed as the width and height parameter
  to
  [`make_page_size()`](https://elipousson.github.io/papersize/reference/make_page_size.md).
  If x is a character string that does not match any of the names in
  paper_sizes, x is passed as the name parameter to
  [`make_page_size()`](https://elipousson.github.io/papersize/reference/make_page_size.md)

- ...:

  Arguments passed on to
  [`make_page_size`](https://elipousson.github.io/papersize/reference/make_page_size.md)

  `width,height`

  :   Page width and height. Both are required, if asp is `NULL`.
      Default to `NULL`.

  `units`

  :   Units for width and height. Required unless units is included in
      dims. Passed to
      [`as_unit_type()`](https://elipousson.github.io/papersize/reference/as_unit.md)
      to validate.

  `asp`

  :   Aspect ratio. Required if only one of width or height are
      provided.

  `name`

  :   Optional name for paper size. Recommend avoiding duplication with
      existing names in `paper_sizes`.

  `dims`

  :   A list or data.frame that can be used to set width, height, units,
      and/or asp.

  `orientation`

  :   Page orientation, Default: `NULL`. Supported options are
      "portrait", "landscape", or "square". If width and height suggest
      a portrait orientation when orientation = "landscape", the
      dimensions are reversed so the page dimensions match the provided
      orientation.

  `valid_units`

  :   Character vector with name or symbols for valid units. Defaults to
      `NULL` but any other unit name or symbol, e.g. "px", is permitted.

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

- cols:

  Column names to use for width and height columns. Defaults to
  c("width", "height"). Must be length 2 and the first value is always
  used as as the width name and the second as the height.

- class:

  Class of return object: "data.frame" (default) or "list" (only
  supported when input page size is a single row).

## Value

A data.frame or list object with one or more page dimensions.

## Examples

``` r
as_page(c(8.5, 11), units = "in")
#>   width height  units orientation       asp
#> 1   8.5     11 inches    portrait 0.7727273

as_page("letter")
#> # A tibble: 1 × 10
#>   name   series size  standard units width height orientation type    asp
#>   <chr>  <chr>  <chr> <chr>    <chr> <dbl>  <dbl> <chr>       <chr> <dbl>
#> 1 Letter NA     NA    ANSI     in      8.5     11 portrait    paper 0.773

as_page(
  list(name = "letter", w = 8.5, h = 11, units = "in"),
  cols = c("w", "h")
)
#>     w  h  units orientation       asp
#> 1 8.5 11 inches    portrait 0.7727273
```
