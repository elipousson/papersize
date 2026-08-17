# Specify the margins of a page or element

An extended version of
[`ggplot2::margin()`](https://ggplot2.tidyverse.org/reference/element.html)
with more flexibility in specification. If margin is a margin class
object it is returned as is. If margin is length 1, the t, r, b, and l
values are all set to that value. If margin is length 2, the t and b are
set to half the first value and the r and l are set to half the second
value. The `...` parameters also allow you to use the same syntax as
margin.

## Usage

``` r
margins(margin = NULL, ..., unit = "in")

is_margin(x)

get_margin(margin = NULL, ..., unit = "in")
```

## Arguments

- margin:

  A numeric list or vector or a margin class object. For `get_margin()`
  only, margin can be a margin name from `page_extras[["margins"]]`.
  Defaults to `NULL`.

- ...:

  Additional numeric values combined with margin if provided.

- unit:

  Default units for margins (ignored if margin is a margin class
  object). Passed to
  [`as_unit_type()`](https://elipousson.github.io/papersize/reference/as_unit.md)
  so units class objects as well as unit names supported
  [`grid::unit()`](https://rdrr.io/r/grid/unit.html) are allowed.
  Defaults to "in" except for
  [`margin()`](https://ggplot2.tidyverse.org/reference/element.html)
  where unit defaults to "pt" to match
  [`ggplot2::margin()`](https://ggplot2.tidyverse.org/reference/element.html).

- x:

  Object to check for class margin and unit

## See also

[`set_page_margin()`](https://elipousson.github.io/papersize/reference/set_page_margin.md)

## Examples

``` r

margins(1, unit = "in")
#> [1] 1inches 1inches 1inches 1inches

margins(c(2, 2), unit = "in")
#> [1] 1inches 1inches 1inches 1inches

margins(list(1, 1, 1.5, 1), unit = "cm")
#> [1] 1cm   1cm   1.5cm 1cm  

margins(t = 1, r = 3, b = 1.5, l = 3, unit = "in")
#> [1] 1inches   3inches   1.5inches 3inches  

standard_margin <- get_margin("standard", unit = "in")

is_margin(standard_margin)
#> [1] TRUE

standard_margin
#> [1] 1inches 1inches 1inches 1inches
```
