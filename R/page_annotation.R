#' @noRd
page_annotation <- function(plots,
                            caption = NULL,
                            pg_start = NULL,
                            pg_prefix = NULL,
                            pg_suffix = ".",
                            pg_sep = NULL,
                            pg_after = TRUE,
                            num_style = NULL,
                            pad = NULL,
                            side = "left",
                            ...) {
  if (is_patchwork(plots)) {
    plots <- list(plots)
  }

  pg <- seq_number(plots, pg_start, pg_prefix, pg_suffix, num_style)

  nm <- names(plots)

  for (i in pg) {
    plots[[i]] <-
      pg_annotation(
        plot = plots[[i]],
        caption = caption,
        pg = i,
        pg_sep = pg_sep,
        pg_after = pg_after,
        ...
      )
  }

  set_names(plots, nm)
}

#' Apply an annotation to a single patchwork object
#'
#' @noRd
pg_annotation <- function(plot,
                          caption = NULL,
                          pg = NULL,
                          pg_sep = NULL,
                          pg_after = TRUE,
                          ...) {
  check_installed("patchwork")

  caption <- caption %||% ""
  pg <- pg %||% 1

  if (pg_after) {
    caption <- paste0(
      caption, pg_sep, pg
    )
  } else {
    caption <- paste0(
      pg, pg_sep, caption
    )
  }

  plot +
    patchwork::plot_annotation(
      caption = caption,
      ...
    )
}
