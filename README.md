
<!-- README.md is generated from README.Rmd. Please edit that file -->

# papersize

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Codecov test
coverage](https://codecov.io/gh/elipousson/papersize/branch/main/graph/badge.svg)](https://app.codecov.io/gh/elipousson/papersize?branch=main)
<!-- badges: end -->

The goal of papersize is to provide you with convenience functions
extending grid, ggplot2, and patchwork to help in sizing plots and files
for printing to paper, postcards, playing cards, and other physical
media.

## Installation

You can install the development version of papersize like so:

``` r
# pak::pkg_install("elipousson/papersize")
```

## Example

``` r
library(papersize)
```

papersize includes data on standard paper sizes including sizes for
papers (ANSI, ISO, and other standards), photo prints, postcards, and
playing cards.

``` r
get_paper("Letter")
#>      name series size standard units width height orientation  type       asp
#> 19 Letter   <NA> <NA>     ANSI    in   8.5     11    portrait paper 0.7727273

get_paper("Letter", orientation = "landscape")
#> # A data frame: 1 × 10
#>   name   series size  standard units width height orientation type    asp
#> * <chr>  <chr>  <chr> <chr>    <chr> <dbl>  <dbl> <chr>       <chr> <dbl>
#> 1 Letter <NA>   <NA>  ANSI     in       11    8.5 landscape   paper  1.29

get_card("Poker")
#> # A data frame: 1 × 10
#>   name       series size  standard units width height orientation type    asp
#> * <chr>      <chr>  <chr> <chr>    <chr> <dbl>  <dbl> <chr>       <chr> <dbl>
#> 1 Poker card <NA>   <NA>  <NA>     in      2.5    3.5 portrait    card  0.714
```

papersize includes plotting functions that create lists of ggplot2 plots
with repeated elements that can be assembled with patchwork into page
layouts for print. For example, `plot_cards()` creates a list of
Poker-card sized plots that can be tiled onto a letter-size patchwork to
save and print.

``` r
papersize <-
  plot_cards(
    "Poker",
    n = 8,
    number = TRUE,
    border = TRUE,
    size = 4,
    linewidth = 0.5,
    text = rep(c("\U2664", "\U2661", "\U2662", "\U2667"), 2),
    color = "yellow"
  )

papersize[[1]]
```

<img src="man/figures/README-plot_cards-1.png" width="100%" />

``` r
page_layout(
  plots = papersize,
  page = "Letter",
  orientation = "landscape"
)
#> ℹ Using `dims` from first plot in `plots`.
#> $`1`
```

<img src="man/figures/README-layout_cards-1.png" width="100%" />

papersize currently has very limited features but additional features
are expected to include better support for multi-page layouts, control
over the position of card elements, preset card formats/designs, and
appropriate handling of cut-lines for DIY card printing.
