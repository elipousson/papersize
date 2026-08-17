# Get a paper or card size based on name, dimensions, orientation, or type

- `get_page_size()` filters `paper_sizes` by one or more variables with
  option to reorient page dimensions or convert page units.

- `get_paper()` is equivalent to `get_page_size()` without the option to
  set units, type, or reorient parameters.

- `get_card()` is equivalent to `get_paper()` with `type = "card"` and
  the string "card" attached to the end of any provided name value
  supporting both "Poker card" and "Poker" as a valid name.

- `get_page_dims()` returns the width and height of a single page.

- `convert_page_units()` uses
  [`convert_unit_type()`](https://elipousson.github.io/papersize/reference/as_unit.md)
  to convert the unit used for page dimensions.

## Usage

``` r
get_page_size(
  name = NULL,
  width = NULL,
  height = NULL,
  orientation = NULL,
  reorient = TRUE,
  units = NULL,
  type = NULL,
  ignore.case = TRUE
)

get_paper(name = NULL, width = NULL, height = NULL, orientation = NULL)

get_card(name = NULL, width = NULL, height = NULL, orientation = NULL)

get_page_dims(
  page = NULL,
  width = NULL,
  height = NULL,
  orientation = NULL,
  cols = c("width", "height"),
  arg = caller_arg(page),
  call = parent.frame(),
  ...
)

convert_page_units(
  page,
  units = NULL,
  valueOnly = TRUE,
  cols = c("width", "height"),
  ...
)
```

## Arguments

- name:

  Page name, e.g. "letter", not case sensitive, Default: `NULL`

- width:

  Page width in "in", "px" or "mm" units. Default: `NULL`

- height:

  Page height in "in", "px" or "mm" units. Default: `NULL`

- orientation:

  Page orientation, Default: `NULL`. Supported options are "portrait",
  "landscape", or "square".

- reorient:

  If `TRUE` and orientation is not `NULL`, flip width and height
  dimensions for any pages that do not match the provided orientation.
  Set `reorient = FALSE` to filter pages by orientation.

- units:

  Units to convert page dimensions to using
  [`convert_unit_type()`](https://elipousson.github.io/papersize/reference/as_unit.md).

- type:

  Page type, Options include "paper", "social", "postcard", "print",
  "card", or "screen". Default: `NULL`

- ignore.case:

  If `FALSE`, filtering for page and type are case sensitive. Defaults
  to `TRUE`.

- page:

  Used by `get_page_dims()`, page is either a character vector passed to
  the name parameter of `get_page_size()`, a data.frame with column
  names matching the cols parameter, or a length 2 numeric vector with
  the page width and height.

- cols:

  Length 2 character vector with column names for page dimensions.
  Defaults to c("width", "height").

- arg, call:

  Passed to
  [`cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html) to
  improve internal error messages.

- ...:

  Additional parameters passed by `get_page_dims()` to `get_page_size()`
  if page is a character object.

- valueOnly:

  Passed to valueOnly parameter of
  [`grid::convertUnit()`](https://rdrr.io/r/grid/grid.convert.html).
  Defaults to `FALSE`.

## Value

A data.frame with page, paper, or card name and dimensions.

## See also

[`make_page_size()`](https://elipousson.github.io/papersize/reference/make_page_size.md)

[`get_social_size()`](https://elipousson.github.io/papersize/reference/get_social_size.md)

## Examples

``` r
get_paper("letter")
#> # A tibble: 1 × 10
#>   name   series size  standard units width height orientation type    asp
#>   <chr>  <chr>  <chr> <chr>    <chr> <dbl>  <dbl> <chr>       <chr> <dbl>
#> 1 Letter NA     NA    ANSI     in      8.5     11 portrait    paper 0.773

get_paper("letter", orientation = "landscape")
#> # A tibble: 1 × 10
#>   name   series size  standard units width height orientation type    asp
#>   <chr>  <chr>  <chr> <chr>    <chr> <dbl>  <dbl> <chr>       <chr> <dbl>
#> 1 Letter NA     NA    ANSI     in       11    8.5 landscape   paper  1.29

get_page_size(orientation = "square", reorient = FALSE)
#> # A tibble: 4 × 10
#>   name         series size  standard units  width height orientation type    asp
#>   <chr>        <chr>  <chr> <chr>    <chr>  <dbl>  <dbl> <chr>       <chr> <dbl>
#> 1 Large Post … NA     NA    British… in      10     10   square      paper     1
#> 2 Instagram p… NA     post  Instagr… px    1080   1080   square      soci…     1
#> 3 C10          C      10    ISO      mm      28     28   square      paper     1
#> 4 Square card  NA     NA    NA       in       2.5    2.5 square      card      1

get_page_size("ledger", units = "cm")
#> # A tibble: 1 × 10
#>   name   series size  standard units width height orientation type    asp
#>   <chr>  <chr>  <chr> <chr>    <chr> <dbl>  <dbl> <chr>       <chr> <dbl>
#> 1 Ledger NA     NA    ANSI     cm     27.9   43.2 portrait    paper 0.647

get_card("Tarot")
#> # A tibble: 1 × 10
#>   name       series size  standard units width height orientation type    asp
#>   <chr>      <chr>  <chr> <chr>    <chr> <dbl>  <dbl> <chr>       <chr> <dbl>
#> 1 Tarot card NA     NA    NA       in     2.75   4.75 portrait    card  0.579

get_page_dims(get_paper("letter"))
#>  width height 
#>    8.5   11.0 

convert_page_units(get_paper("letter"), units = "cm")
#> # A tibble: 1 × 10
#>   name   series size  standard units width height orientation type    asp
#>   <chr>  <chr>  <chr> <chr>    <chr> <dbl>  <dbl> <chr>       <chr> <dbl>
#> 1 Letter NA     NA    ANSI     cm     21.6   27.9 portrait    paper 0.773
```
