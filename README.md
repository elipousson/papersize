
<!-- README.md is generated from README.Rmd. Please edit that file -->

# papersize

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://cran.r-project.org/web/licenses/MIT)
[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)\`
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
#> # A tibble: 1 × 9
#>   name      series size   standard units width height orientation type 
#>   <list>    <chr>  <list> <chr>    <chr> <dbl>  <dbl> <chr>       <chr>
#> 1 <chr [1]> <NA>   <NULL> ANSI     in      8.5     11 portrait    paper

get_paper("Letter", orientation = "landscape")
#> # A tibble: 1 × 9
#>   name      series size   standard units width height orientation type 
#>   <list>    <chr>  <list> <chr>    <chr> <dbl>  <dbl> <chr>       <chr>
#> 1 <chr [1]> <NA>   <NULL> ANSI     in       11    8.5 landscape   paper

get_card("Poker")
#> # A tibble: 1 × 9
#>   name      series size   standard units width height orientation type 
#>   <list>    <chr>  <list> <chr>    <chr> <dbl>  <dbl> <chr>       <chr>
#> 1 <chr [1]> <NA>   <NULL> <NA>     in      2.5    3.5 portrait    card
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
layout_cards(
  papersize,
  paper = "Letter",
  orientation = "landscape"
  )
```

<img src="man/figures/README-layout_cards-1.png" width="100%" />

papersize currently has very limited features but additional features
are expected to include better support for multi-page layouts, control
over the position of card elements, preset card formats/designs, and
appropriate handling of cut-lines for DIY card printing.
