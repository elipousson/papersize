# General utility functions for working with distance units objects

- `is_dist_units()`: Is x a units object with a units attribute in
  `dist_unit_options` or `area_unit_options`?

- `get_dist_units()`: Get the distance units from x (if x is a sf or
  units objects or a character string from
  [dist_unit_options](https://elipousson.github.io/papersize/reference/dist_unit_options.md))

- `as_dist_units()`: Convert x to units using
  [units::as_units](https://r-quantities.github.io/units/reference/units.html)

- `is_same_units()`: Do x and y have the same distance units attribute?
  Names or symbols of valid distance units are allowed.

## Usage

``` r
is_dist_units(x, arg = caller_arg(x))

get_dist_units(x, arg = caller_arg(x), call = parent.frame())

as_dist_units(x, units = NULL, arg = caller_arg(x), call = parent.frame())

is_same_units(x, y = NULL)
```

## Arguments

- x, y:

  objects to check

- arg:

  Used internally and passed to
  [`rlang::arg_match()`](https://rlang.r-lib.org/reference/arg_match.html)
  as error arg or used by
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html)
  to improve error messages.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- units:

  Distance units to convert to. Must be one of dist_unit_options or
  area_unit_options.

## See also

Other dist:
[`convert_dist_scale()`](https://elipousson.github.io/papersize/reference/convert_dist_scale.md),
[`convert_dist_units()`](https://elipousson.github.io/papersize/reference/convert_dist_units.md)

## Examples

``` r

mile <- units::set_units(1, "mi")

is_dist_units("mi")
#> [1] FALSE

is_dist_units(mile)
#> [1] TRUE

is_same_units(mile, "mile")
#> [1] TRUE

get_dist_units(mile)
#> [1] "mi"

as_dist_units(1, "mi")
#> 1 [mi]

as_dist_units(2, mile)
#> 2 [mi]
```
