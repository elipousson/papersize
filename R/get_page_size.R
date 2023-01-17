#' Get a paper or card size based on name, dimensions, orientation, or type
#'
#' @description
#'
#' - [get_page_size()] filters `paper_sizes` by one or more variables with option to
#' reorient page dimensions or convert page units.
#' - [get_paper()] is equivalent to [get_page_size()] without the option to set
#' units, type, or reorient parameters.
#' - [get_card()] is equivalent to [get_paper()] with `type = "card"` and the
#' string "card" attached to the end of any provided name value supporting both
#' "Poker card" and "Poker" as a valid name.
#' - [get_page_dims()] returns the width and height of a single page.
#' - [convert_page_units()] uses [convert_unit_type()] to convert the unit used
#' for page dimensions.
#'
#' @name get_page_size
#' @param name Page name, e.g. "letter", not case sensitive, Default: `NULL`
#' @param width Page width in "in", "px" or "mm" units. Default: `NULL`
#' @param height Page height in "in", "px" or "mm" units. Default: `NULL`
#' @param orientation Page orientation, Default: `NULL`. Supported options are
#'   "portrait", "landscape", or "square".
#' @param reorient If `TRUE` and orientation is not `NULL`, flip width and height
#'   dimensions for any pages that do not match the provided orientation. Set
#'   `reorient = FALSE` to filter pages by orientation.
#' @param type Page type, Options include "paper", "social", "postcard",
#'   "print", "card", or "screen". Default: `NULL`
#' @param ignore.case If `FALSE`, filtering for page and type are case
#'   sensitive. Defaults to `TRUE`.
#' @examples
#' get_paper("letter")
#'
#' get_paper("letter", orientation = "landscape")
#'
#' get_page_size(orientation = "square", reorient = FALSE)
#'
#' get_page_size("ledger", units = "cm")
#'
#' get_card("Tarot")
#'
#' @seealso [make_page_size()]
#' @seealso [get_social_size()]
#' @return A data.frame with page, paper, or card name and dimensions.
#' @export
#' @importFrom rlang arg_match
get_page_size <- function(name = NULL,
                          width = NULL,
                          height = NULL,
                          orientation = NULL,
                          reorient = TRUE,
                          units = NULL,
                          type = NULL,
                          ignore.case = TRUE) {
  pg <- paper_sizes

  if (!is.null(name) && !(tolower(name) %in% tolower(pg$name))) {
    name <- rlang::arg_match(
      name,
      values = as.character(pg$name),
      multiple = TRUE
    )
  }

  pg <- filter_data(pg, name, ignore.case = ignore.case)

  pg <- filter_data(pg, width)

  pg <- filter_data(pg, height)

  if (!is.null(type) && !(tolower(type) %in% tolower(pg$type))) {
    type <- rlang::arg_match(
      type,
      values = as.character(pg$type),
      multiple = TRUE
    )
  }

  pg <- filter_data(pg, type, ignore.case = ignore.case)

  if (!is.null(units)) {
    pg <- convert_page_units(pg, units)
  }

  if (is.null(orientation)) {
    return(set_page_asp(pg))
  }

  if (!reorient) {
    orientation <- tolower(orientation)

    orientation <-
      rlang::arg_match(
        orientation,
        c("portrait", "landscape", "square"),
        multiple = TRUE
      )

    pg <- filter_data(pg, orientation)

    return(set_page_asp(pg))
  }

  pg <- set_page_orientation(pg, orientation)

  set_page_asp(pg)
}

#' @name get_paper
#' @rdname get_page_size
#' @export
get_paper <- function(name = NULL,
                      width = NULL,
                      height = NULL,
                      orientation = NULL) {
  get_page_size(
    name = name,
    width = width,
    height = height,
    orientation = orientation
  )
}

#' @name get_card
#' @rdname get_page_size
#' @export
get_card <- function(name = NULL,
                     width = NULL,
                     height = NULL,
                     orientation = NULL) {
  if (!is.null(name) && !str_detect(name, " card$")) {
    name <- paste(name, "card")
  }

  get_page_size(
    name = name,
    width = width,
    height = height,
    orientation = orientation,
    type = "card"
  )
}

#' @name get_page_dims
#' @rdname get_page_size
#' @param page Used by [get_page_dims()], page is either a character vector
#'   passed to the name parameter of [get_page_size()], a data.frame with column
#'   names matching the cols parameter, or a length 2 numeric vector with the
#'   page width and height.
#' @param cols Length 2 character vector with column names for page dimensions.
#'   Defaults to c("width", "height").
#' @param arg,call Passed to [cli_abort()] to improve internal error messages.
#' @param ... Additional parameters passed by [get_page_dims()] to [get_page_size()]
#'   if page is a character object.
#' @export
#' @importFrom rlang caller_arg
#' @importFrom cli cli_abort
get_page_dims <- function(page = NULL,
                          width = NULL,
                          height = NULL,
                          orientation = NULL,
                          cols = c("width", "height"),
                          arg = caller_arg(page),
                          call = parent.frame(),
                          ...) {
  page <-
    as_page(
      page,
      width = width,
      height = height,
      orientation = orientation,
      cols = cols,
      ...
    )

  if (is.data.frame(page)) {
    check_page(page, cols[1:2], n = 1, call = call)

    return(rlang::set_names(c(page[[cols[1]]], page[[cols[2]]]), cols[1:2]))
  }

  nm <- check_dims_cols(cols, width, height)

  if (all(is.numeric(c(width, height)))) {
    return(rlang::set_names(c(width, height), nm))
  }

  if (is.numeric(page) && (length(page) == 2)) {
    return(rlang::set_names(page, nm))
  }

  cli::cli_abort(
    "{.arg {arg}} must be a character with a name from {.code paper_sizes},
    a {.cls data.frame} with columns {.val {cols}},
    or a {.cls numeric} vector in the form {.code c(<width>, <height>)}.",
    call = call
  )
}

#' @noRd
check_dims_cols <- function(cols = c("width", "height"),
                            width = NULL,
                            height = NULL,
                            default = c("width", "height")) {
  if (identical(cols, default)) {
    return(cols)
  }

  if (all(is.numeric(c(width, height)))) {
    cli::cli_warn(
      c("{cols} must be {.val {default}} when {.arg width} and {.arg height} are provided.",
        "i" = "Replacing {.val {cols}} with {.val {default}}."
      )
    )

    return(default)
  }

  if ((length(cols) != 2) | !is.character(cols)) {
    cli::cli_warn(
      c(
        "{.arg cols} must be a length 2 {.cls character} vector.",
        "i" = "Replacing {.val {cols}} with {.val {default}}."
      )
    )

    return(default)
  }

  cols
}

#' @name convert_page_units
#' @rdname get_page_size
#' @param units Units to convert page dimensions to using [convert_unit_type()].
#' @export
convert_page_units <- function(page,
                               units = NULL,
                               cols = c("width", "height")) {
  if (is.null(units)) {
    return(page)
  }

  units_col <- get_units_col()

  cli_abort_ifnot(
    "{.arg page} must have a name {.val units_col}" = rlang::has_name(page, units_col)
  )

  if (is_same_unit_type(page[[units_col]], units)) {
    return(page)
  }

  pg_dims <-
    convert_unit_type(
      c(page[[cols[1]]], page[[cols[2]]]),
      page[[units_col]],
      units,
      valueOnly = TRUE
    )

  set_page_dims(
    page,
    dims = pg_dims,
    units = units
  )
}

#' @noRd
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

#' @noRd
#' @importFrom cli cli_abort
#' @importFrom rlang caller_arg has_name
check_page <- function(page,
                       cols = c("width", "height"),
                       n = NULL,
                       arg = caller_arg(page),
                       call = parent.frame()) {
  if ((!is.data.frame(page) & !is.list(page)) | !all(rlang::has_name(page, cols))) {
    cli::cli_abort(
      "{.arg {arg}} must be a {.cls data.frame} or {.cls list} with columns
      or names {.val {cols}}.",
      call = call
    )
  }

  pg_n <- nrow(page)

  if (is.data.frame(page) && !is.null(n) && (pg_n > n)) {
    cli::cli_abort(
      "{.arg {arg}} must have no more than {.val {n}} rows,
      and {.arg {arg}} has {.val {pg_n}}.",
      call = call
    )
  }
}

#' @noRd
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

  units_col <- get_units_col()

  page[[cols[1]]] <- width %||% page[[cols[1]]]
  page[[cols[2]]] <- height %||% page[[cols[2]]]
  page[[units_col]] <- units %||% page[[units_col]]

  page
}

#' Apply numeric or unit class inset to width/height dimensions of page
#'
#' @noRd
inset_page <- function(page,
                       inset = 0.1,
                       units = "in",
                       pct_inset = NULL,
                       cols = c("width", "height")) {
  units_col <- get_units_col()

  if (!is_unit(inset) && is.null(pct_inset)) {
    inset <- as_unit(inset, units)
  }

  if (is_unit(inset)) {
    dims <- get_inset_dims(c(page[[cols[1]]], page[[cols[2]]]), page[[units_col]], inset)
    return(set_page_dims(page, dims, cols = cols))
  }

  if (length(pct_inset) == 1) {
    pct_inset <- rep(pct_inset, 2)
  }

  set_page_dims(
    page,
    width = page[[cols[1]]] * (1 - pct_inset[[1]]),
    height = page[[cols[2]]] * (1 - pct_inset[[2]]),
    cols = cols
  )
}

#' Apply unit class inset to width/height dimensions
#'
#' @noRd
#' @importFrom rlang set_names
get_inset_dims <- function(dims,
                           units = "in",
                           inset = grid::unit(1, "in"),
                           nm = c("width", "height")) {
  rlang::check_installed("grid")

  inset <- convert_unit_type(inset, to = units, valueOnly = TRUE)

  if (length(inset) == 1) {
    inset <- rep(inset, 2)
  }

  rlang::set_names(dims - (inset * 2), nm)
}

#' Append an aspect ratio column a page size data.frame
#'
#' @noRd
set_page_asp <- function(page,
                         cols = c("width", "height")) {
  asp_col <- get_asp_col()

  if (rlang::has_name(page, asp_col)) {
    return(page)
  }

  page[[asp_col]] <- page[[cols[1]]] / page[[cols[2]]]
  page
}
