# Calculate the thickness of a stack of cards or paper

`as_thickness()` estimates the thickness of a stack of `n` sheets of
paper or cards, each `pt` in caliper thickness (in points, i.e.
thousandths of an inch). Alternatively, supply a known thickness
directly with `x`. Used by
[`plot_band()`](https://elipousson.github.io/papersize/reference/plot_band.md)
to size the folded sides of a band that wraps around a stack.

## Usage

``` r
as_thickness(..., n = NULL, pt = 10, x = NULL, units = NULL)
```

## Arguments

- ...:

  Not used.

- n:

  Number of sheets or cards in the stack. Ignored if `x` is supplied.
  Either `n` or `x` must be supplied.

- pt:

  Caliper thickness of a single sheet or card, in points (1/1000 in).
  Ignored if `x` is supplied. Default: 10, a typical thickness for a
  playing card.

- x:

  Optional. A known stack thickness (numeric or `unit` object). If
  supplied, `n` and `pt` are ignored.

- units:

  A character vector specifying the units for the corresponding numeric
  values.

## Value

A `unit` object with the thickness of the stack.

## Examples

``` r
as_thickness(n = 54)
#> [1] 0.54inches

as_thickness(x = 0.75, units = "in")
#> [1] 0.75inches
```
