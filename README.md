
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cards

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://cran.r-project.org/web/licenses/MIT)
[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

The goal of cards is to …

## Installation

You can install the development version of cards like so:

``` r
# pak::pkg_install("elipousson/cards")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(cards)
```

cards includes data on standard page sizes including sizes for papers,
photo prints, postcards, and playing cards.

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

cards uses ggplot2 to create plots for individual cards and patchwork to
assemble card plots into page layouts for print.

``` r
cards <-
  plot_cards(
    "Poker",
    n = 8,
    number = TRUE,
    border = TRUE,
    size = 4,
    linewidth = 0.5,
    text = rep(c("\U2664", "\U2661", "\U2662", "\U2667"), 2), # "♤", "♡", "♢", "♧" 
    color = "yellow"
    )

cards[[1]]
```

<img src="man/figures/README-plot_cards-1.png" width="100%" />

``` r
layout_cards(cards, paper = "Letter", orientation = "landscape")
```

<img src="man/figures/README-layout_cards-1.png" width="100%" />

cards currently has very limited features but additional features are
expected to include multi-page layouts, control over the position of
card elements, preset card formats/designs, and appropriate handling of
cut-lines for DIY card printing.
