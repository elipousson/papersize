# Use ggplot to plot for one or more cards

`plot_cards()` takes a card that defines the dimensions of a card and
creates a list of plots using the specified aesthetics.

## Usage

``` r
plot_cards(
  card,
  n = 1,
  orientation = "portrait",
  number = FALSE,
  color = "white",
  size = 5,
  family = NULL,
  fill = "gray20",
  border = FALSE,
  inset = grid::unit(c(5, 5), "mm"),
  linetype = "dashed",
  linewidth = 1,
  text = NULL,
  center = NULL
)
```

## Arguments

- card:

  Card name or data.frame with width and height columns.

- n:

  Number of cards to plot, Default: 1

- orientation:

  Card orientation, Default: 'portrait'

- number:

  If `TRUE`, add a number to each card, Default: `FALSE`

- color:

  Color for number, text, and border, Default: 'white'

- size:

  Font size for number and text, Default: 5

- family:

  Font family for number and text, Default: 'Georgia'

- fill:

  Length 1 or 2 character vector with color name. If length 2, the first
  value is assumed to be the card fill and the second value is assumed
  to be the inset border fill. Default: 'gray20'

- border:

  If TRUE, add a border to the card. Default: FALSE

- inset:

  Unit or numeric vector with inset distance for card border, Default:
  `grid::unit(c(5, 5), "mm")`. If inset is a numeric vector, it is
  expected to be a percent relative to the card width and height.

- linetype:

  linetype for card border, Default: 'dashed'

- linewidth:

  linewidth for card border, Default: 2

- text:

  Character vector with card text, Default: NULL

- center:

  Position of card center, Default: c(0, 0)

## Value

A list of plot objects where each item on the list is a card.

## Examples

``` r
#'
if (FALSE) { # \dontrun{
if (interactive() && is_installed("ggplot2")) {
  plot_cards("Tarot", n = 2, number = TRUE)[[2]]

  plot_cards("Poker", n = 1, number = TRUE, text = "♡️")
}
} # }
```
