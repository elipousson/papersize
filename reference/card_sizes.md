# Standard card sizes

Reference table of common playing card sizes for
[`get_card()`](https://elipousson.github.io/papersize/reference/get_page_size.md).
Data is a subset of `paper_sizes` which is also included with `{sfext}`
package.

## Usage

``` r
card_sizes
```

## Format

A data frame with 5 rows and 6 variables:

- `name`:

  Name of card

- `units`:

  Units ("in" or "mm") for dimensions

- `width`:

  Width in units

- `height`:

  Height in units

- `orientation`:

  Portrait (width less than height), landscape, or square
