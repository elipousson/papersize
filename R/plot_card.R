#' Use ggplot to plot for one or more cards
#'
#' Make a plot of cards.
#'
#' @param card Card name or data.frame with width and height columns.
#' @param n Number of cards to plot, Default: 1
#' @param orientation Card orientation, Default: 'portrait'
#' @param number If `TRUE`, add a number to each card, Default: `FALSE`
#' @param color Color for number, text, and border, Default: 'white'
#' @param size Font size for number and text, Default: 5
#' @param family Font family for number and text, Default: 'Georgia'
#' @param fill Length 1 or 2 character vector with color name. If length 2, the
#'   first value is assumed to be the card fill and the second value is assumed
#'   to be the inset border fill. Default: 'gray20'
#' @param border If TRUE, add a border to the card. Default: FALSE
#' @param inset Unit or numeric vector with inset distance for card border,
#'   Default: unit(c(5, 5), "mm"). If inset is a numeric vector, it is expected
#'   to be a percent relative to the card width and height.
#' @param linetype linetype for card border, Default: 'dashed'
#' @param linewidth linewidth for card border, Default: 2
#' @param text Character vector with card text, Default: NULL
#' @param center Position of card center, Default: c(0, 0)
#' @return A list of plot objects where each item on the list is a card.
#' @examples
#' plot_cards("Tarot", n = 2, number = TRUE)[[2]]
#'
#' plot_cards("Poker", n = 1, number = TRUE, text = "♡️")
#' @rdname plot_cards
#' @export
plot_cards <- function(card,
                       n = 1,
                       orientation = "portrait",
                       number = FALSE,
                       color = "white",
                       size = 5,
                       family = NULL,
                       fill = "gray20",
                       border = FALSE,
                       inset = unit(c(5, 5), "mm"),
                       linetype = "dashed",
                       linewidth = 1,
                       text = NULL,
                       center = NULL) {
  if (is.character(card)) {
    card <- get_card(card, orientation = orientation)
  }

  if (!all(rlang::has_name(card, c("x", "y")))) {
    center <- center %||% c(0, 0)
    card$x <- center[1]
    card$y <- center[2]
  }

  plots <- make_card_plots(card, n, fill)

  if (border) {
    plots <- add_card_border(
      plots,
      card,
      inset,
      fill,
      color,
      linetype,
      linewidth
    )
  }

  if (number) {
    plots <- add_card_number(plots, card, n, color, size, family)
  }

  if (!is.null(text)) {
    plots <- add_card_text(plots, card, text, color, size, family)
  }

  plots
}

#' Helper to create a list of plots for n cards
#'
#' @noRd
make_card_plots <- function(card,
                            n = 1,
                            fill = "gray20",
                            fixed = TRUE) {
  n <- c(1:n)

  if (length(fill) == 2) {
    fill <- fill[1]
  }

  map(
    n,
    function(x) {
      setup_card_plot(card, fill, fixed = TRUE)
    }
  )
}

#' Helper to create a ggplot for a single card
#'
#' @noRd
#' @importFrom ggplot2 ggplot geom_tile aes theme_void coord_fixed
setup_card_plot <- function(card,
                            fill = "gray20",
                            tile = TRUE,
                            fixed = TRUE) {
  check_page(card, cols = c("width", "height", "x", "y"))

  plot <-
    ggplot2::ggplot()

  if (tile) {
    plot <- plot +
      ggplot2::geom_tile(
        data = card,
        ggplot2::aes(x = x, y = y, width = width, height = height),
        fill = fill
      )
  }

  plot <- plot +
    ggplot2::theme_void()

  if (fixed) {
    return(plot + ggplot2::coord_fixed())
  }

  plot
}

#' Helper to add a number to each card plot in a list
#'
#' @noRd
#' @importFrom ggplot2 geom_text aes
add_card_number <- function(plots,
                            card = NULL,
                            n = 1,
                            color = "white",
                            size = 5,
                            family = NULL) {
  n <- c(1:n)

  # repeat card function to copy layout rows

  map(
    n,
    function(i) {
      card_number <-
        data.frame(
          "x" = card$x,
          "y" = card$y,
          "label" = i
        )

      plots[[i]] +
        geom_text_if_family(
          data = card_number,
          mapping = ggplot2::aes(x = x, y = y, label = label),
          color = color, size = size, family = family
        )
    }
  )
}

#' Helper to add a border to each card plot in a list
#'
#' @noRd
#' @importFrom grid unit
#' @importFrom ggplot2 geom_tile aes
add_card_border <- function(plots,
                            card = NULL,
                            inset = unit(c(5, 5), "mm"),
                            fill = NA,
                            color = "white",
                            linetype = "dashed",
                            linewidth = 1) {
  inset_card <- inset_page(card, inset)

  if (length(fill) == 2) {
    fill <- fill[2]
  } else {
    fill <- NA
  }

  map_gg(
    plots,
    ggplot2::geom_tile(
      data = inset_card,
      ggplot2::aes(
        x = x, y = y,
        width = width,
        height = height
      ),
      fill = fill,
      color = color,
      linetype = linetype,
      linewidth = linewidth
    )
  )
}

#' Helper to add text to each card plot in a list
#'
#' @noRd
#' @importFrom ggplot2 geom_text aes
add_card_text <- function(plots,
                          card = NULL,
                          text = NULL,
                          color = "white",
                          size = 5,
                          family = NULL,
                          nudge_x = 0,
                          nudge_y = 0.5) {
  if (is.null(text)) {
    return(plots)
  }

  n <- length(plots)

  if (is.character(text)) {
    if (length(text) == 1) {
      text <- rep(text, n)
    }

    stopifnot(
      length(text) == n
    )

    text <-
      data.frame(
        "label" = text,
        "x" = rep(card$x + nudge_x, n),
        "y" = rep(card$y + nudge_y, n)
      )

    plots <-
      map(
        c(1:n),
        function(i) {
          plots[[i]] +
            geom_text_if_family(
              data = text[i, ],
              mapping = ggplot2::aes(
                x = x,
                y = y,
                label = label
              ),
              size = size,
              color = color,
              family = family
            )
        }
      )

    return(plots)
  }

  # if (is.data.frame(text)) {
  #
  # }
}
