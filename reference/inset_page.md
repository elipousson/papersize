# Apply numeric or unit class inset to width/height dimensions of page

`inset_page()` applies a set inset distance value or percent distance to
modify the dimensions of the input page list or data frame.

## Usage

``` r
inset_page(
  page,
  inset = 0.1,
  units = "in",
  pct_inset = NULL,
  cols = c("width", "height")
)
```

## Arguments

- inset:

  Numeric distance to inset an input page.

- units:

  A character vector specifying the units for the corresponding numeric
  values.

- pct_inset:

  Percent inset based on existing page dimensions.
