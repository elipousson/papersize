#' Use patchwork to layout cards on a paper
#'
#' @param card Card name, a data.frame with width and height columns, or a list
#'   of ggplot2 objects with card plots. Default: `NULL`
#' @param paper Paper name or a data.frame with width and height columns.
#'   Optional if width and height are both provided, Default: `NULL`
#' @param width,height Paper width and height, Default: `NULL`
#' @param orientation Paper orientation, Optional if width and height are both
#'   provided, Default: 'landscape'
#' @inheritParams patchwork::wrap_plots
#' @inheritDotParams patchwork::wrap_plots -ncol -nrow
#' @return A `patchwork` object
#' @examples
#' layout_cards(
#'   card = plot_cards("Poker", 6),
#'   paper = "letter"
#' )
#' @seealso
#'  [ggplot2::ggplot_build()]
#'  [patchwork::wrap_plots()], [patchwork::plot_layout()]
#' @rdname layout_cards
#' @export
#' @importFrom ggplot2 layer_data
#' @importFrom patchwork wrap_plots plot_layout
layout_cards <- function(card = NULL,
                         paper = NULL,
                         width = NULL,
                         height = NULL,
                         orientation = "landscape",
                         byrow = FALSE,
                         ...) {
  paper_dims <- get_page_dims(paper, width, height, orientation)

  if (is.list(card)) {
    ncol <- NULL
    nrow <- NULL

    if (is.numeric(paper_dims) && is_gg(card[[1]])) {
      card_dims <- ggplot2::layer_data(card[[1]])

      card_dims$width <- abs(diff(c(card_dims$xmin, card_dims$xmax)))
      card_dims$height <- abs(diff(c(card_dims$ymin, card_dims$ymax)))

      ncol <- paper_dims[[1]] %/% card_dims$width
      nrow <- paper_dims[[2]] %/% card_dims$height
    }

    return(
      patchwork::wrap_plots(
        card,
        ncol = ncol,
        nrow = nrow,
        byrow = byrow,
        ...
      )
    )
  }

  dims <- get_page_dims(card)

  patchwork::plot_layout(
    ncol = paper_dims[[1]] %/% dims[[1]],
    nrow = paper_dims[[2]] %/% dims[[2]],
    byrow = byrow,
    ...
  )
}
