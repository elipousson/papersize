# Convert distance (and area) values between different units

Convert distance (and area) values between different units

## Usage

``` r
convert_dist_units(
  dist,
  from = NULL,
  to = "meter",
  drop = FALSE,
  digits = NULL
)
```

## Arguments

- dist:

  Numeric or units object

- from:

  Existing unit for dist, Default: `NULL`. If dist is a units object,
  the numerator is used as "from"

- to:

  Unit to convert distance to, Default: 'meter'

- drop:

  If `TRUE`, return numeric. If `FALSE`, return class units object.

- digits:

  Number of digits to include in result; defaults to `NULL`.

## Value

Object created by
[`units::set_units()`](https://r-quantities.github.io/units/reference/units.html)

## See also

[`is_same_unit_type()`](https://elipousson.github.io/papersize/reference/as_unit.md)

Other dist:
[`convert_dist_scale()`](https://elipousson.github.io/papersize/reference/convert_dist_scale.md),
[`is_dist_units()`](https://elipousson.github.io/papersize/reference/is_dist_units.md)

## Examples

``` r
convert_dist_units(1, from = "mile", to = "km")
#> 1.609344 [km]

convert_dist_units(3, from = "ft", to = "yard")
#> 1 [yard]

mile <- units::set_units(1, "mi")

convert_dist_units(mile, to = "feet")
#> 5280 [feet]

is_same_units(mile, "mile")
#> [1] TRUE
```
