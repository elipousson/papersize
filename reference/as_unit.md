# Helper functions for grid units

- `as_unit()`: Convert to a unit (allowing unit objects as units)

- `as_unit_type()`: Convert to unit type (or checking unit types)

- `convert_unit_type()`: Convert x from one unit type to another
  (preserving names for named vectors)

- `is_unit_type()`: Is x a character vector with a unit type supported
  by the grid package or a unit object with a supported type?

- `is_same_unit_type()`: Are x and y the same unit type?

Note, when `as_unit_type()` is used on a margin object, it returns the
unique unit type as a length 1 character vector not a length 4 character
vector as you could expect with other length 4 input objects.

## Usage

``` r
as_unit(
  x,
  units = NULL,
  data = NULL,
  recurse = FALSE,
  arg = caller_arg(x),
  call = parent.frame()
)

as_unit_type(
  x,
  recurse = FALSE,
  data = NULL,
  valid_units = NULL,
  arg = caller_arg(x),
  call = parent.frame()
)

convert_unit_type(
  x,
  from = NULL,
  to = NULL,
  typeFrom = "dimension",
  valueOnly = FALSE,
  ...
)

is_unit_type(x, ...)

is_same_unit_type(x, y, recurse = FALSE, data = NULL, valid_units = NULL)
```

## Arguments

- x:

  A numeric vector.

  For `is.unit`, any R object.

- units:

  A character vector specifying the units for the corresponding numeric
  values.

- data:

  This argument is used to supply extra information for special `unit`
  types.

- recurse:

  Whether to recurse into complex units.

- arg:

  Passed to
  [`cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html) to
  improve internal error messages.

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

- valid_units:

  Character vector with name or symbols for valid units. Defaults to
  `NULL` but any other unit name or symbol, e.g. "px", is permitted.

- from:

  Unit to convert from. If `NULL` and x is not a units object, convert
  to `to` units with a warning.

- to:

  Unit to convert to. Passed to unitTo parameter of
  [`grid::convertUnit()`](https://rdrr.io/r/grid/grid.convert.html). If
  `NULL`, return x as is.

- typeFrom:

  Passed to typeFrom parameter of
  [`grid::convertUnit()`](https://rdrr.io/r/grid/grid.convert.html).
  Defaults to "dimension".

- valueOnly:

  Passed to valueOnly parameter of
  [`grid::convertUnit()`](https://rdrr.io/r/grid/grid.convert.html).
  Defaults to `FALSE`.

- ...:

  Arguments passed on to
  [`grid::convertUnit`](https://rdrr.io/r/grid/grid.convert.html)

  `axisFrom`

  :   Either `"x"` or `"y"` to indicate whether the unit object
      represents a value in the x- or y-direction.

  `axisTo`

  :   Same as `axisFrom`, but applies to the unit object that is to be
      created.

  `typeTo`

  :   Same as `typeFrom`, but applies to the unit object that is to be
      created.

- y:

  Object to compare to x.

## Value

A `unit` class object, a character vector with a unit type, or a logical
vector.

## Examples

``` r

inch <- as_unit(1, "in")

inch
#> [1] 1inches

as_unit(10, inch)
#> [1] 10inches

as_unit(inch, "cm")
#> [1] 1cm

as_unit(inch)
#> [1] 1inches

convert_unit_type(inch, to = "cm")
#> [1] 2.54cm

convert_unit_type(c(10, 100), from = "mm", to = "cm")
#> [1] 1cm  10cm

is_unit_type("inch")
#> [1] TRUE

is_unit_type("inchs")
#> [1] FALSE

is_same_unit_type(inch, "in")
#> [1] TRUE

is_same_unit_type("pt", "points")
#> [1] TRUE
```
