#' Set page data.frame dimensions, orientation, or aspect ratio
#'
#' @name set_page_dims
#' @export
#' @importFrom rlang is_named
set_page_dims <- function(page,
                          dims = NULL,
                          width = NULL,
                          height = NULL,
                          units = NULL,
                          cols = c("width", "height")) {
  if (!is.null(dims)) {
    stopifnot(length(dims) == 2)

    if (rlang::is_named(dims)) {
      dims <- c(dims[cols[1]], dims[cols[2]])
    }

    width <- dims[[1]]
    height <- dims[[2]]
  }

  if (all(is_unit_type(page[[cols[1]]]))) {
    units <- unique(as_unit_type(page[[cols[1]]]))
  }

  units_col <- get_units_col()

  page[[cols[1]]] <- width %||% page[[cols[1]]]
  page[[cols[2]]] <- height %||% page[[cols[2]]]
  page[[units_col]] <- units %||% page[[units_col]]

  page
}

#' @name set_page_orientation
#' @rdname set_page_dims
#' @export
#' @importFrom rlang has_name set_names arg_match
#' @importFrom cli cli_warn
set_page_orientation <- function(page,
                                 orientation = NULL,
                                 tolerance = 0.1,
                                 cols = c("width", "height")) {
  has_orientation_col <- rlang::has_name(page, get_orientation_col())

  if (is.null(orientation) & has_orientation_col) {
    return(page)
  }

  pg_orientation <- as_orientation(page[[cols[1]]] / page[[cols[2]]], tolerance)

  if (!has_orientation_col) {
    page <-
      cbind(
        page,
        rlang::set_names(data.frame(pg_orientation), get_orientation_col())
      )

    if (is.null(orientation)) {
      return(page)
    }
  }

  orientation <- tolower(orientation)

  orientation <-
    rlang::arg_match(
      orientation,
      c("portrait", "landscape", "square"),
      multiple = TRUE
    )

  switch_pg <- !(orientation == pg_orientation)

  if (!any(switch_pg)) {
    return(page)
  }

  pg_in <- page

  page[switch_pg, ] <-
    set_page_dims(
      page[switch_pg, ],
      width = page[switch_pg, ][[cols[2]]],
      height = page[switch_pg, ][[cols[1]]],
      cols = cols
    )

  orientation_col <- get_orientation_col()

  page[switch_pg, ][[orientation_col]] <-
    as_orientation(
      page[switch_pg, ][[cols[1]]] / page[switch_pg, ][[cols[2]]],
      tolerance,
      cols = cols
    )

  if (any(!(orientation == page[[orientation_col]]))) {
    cli::cli_warn(
      c("{.arg orientation} can't be set to {.val {orientation}}
        when the page width is {.val {pg_in[[cols[1]]]}} and height
        is {.val {pg_in[[cols[2]]]}}.",
        "i" = "Orientation kept as {.val {pg_in$orientation}}."
      )
    )

    return(pg_in)
  }

  page
}

#' set_page_asp: appends an aspect ratio column a page size data.frame
#'
#' @rdname set_page_dims
#' @name set_page_asp
#' @export
#' @importFrom rlang has_name
set_page_asp <- function(page,
                         flipped = FALSE,
                         cols = c("width", "height")) {
  page[[get_asp_col()]] <-
    as_asp(
      page = page,
      flipped = flipped,
      cols = cols
    )
  page
}
