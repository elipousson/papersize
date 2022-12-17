#' Get a paper or card size based on name, dimensions, orientation, or type
#'
#' @description
#'
#' - [get_page()] filters `page_sizes` by one or more variables with option to
#' reorient page dimensions or convert page units.
#' - [get_paper()] is equivalent to [get_page()] without the option to set units,
#' type, or reorient parameters.
#' - [get_card()] is equivalent to [get_paper()] with `type = "card"` and the
#' string "card" attached to the end of any provided name value.
#' - [get_page_dims()] returns the width and height of a single page.
#' - [convert_page_units()] uses [convert_unit_type()] to convert the unit used
#' for page dimensions.
#'
#' @name get_page
#' @param name Page name, e.g. "letter", not case sensitive, Default: NULL
#' @param width Page width in "in", "px" or "mm" units. Default: NULL
#' @param height Page height in "in", "px" or "mm" units. Default: NULL
#' @param orientation Page orientation, Default: NULL
#' @param reorient If `TRUE` and orientation is not NULL, flip width and height
#'   dimensions for any pages that do not match the provided orientation. Set
#'   `reorient = FALSE` to filter pages by orientation.
#' @param type Page type, Options include "paper", "social", "postcard",
#'   "print", "card", or "screen". Default: NULL
#' @return A data.frame with page, paper, or card name and dimensions.
#' @examples
#' get_paper("letter")
#'
#' get_paper("letter", orientation = "landscape")
#'
#' get_page(orientation = "square", reorient = FALSE)
#'
#' get_page("ledger", units = "cm")
#'
#' get_card("Tarot")
#'
#' @export
#' @importFrom rlang arg_match
get_page <- function(name = NULL,
                     width = NULL,
                     height = NULL,
                     orientation = NULL,
                     reorient = TRUE,
                     units = NULL,
                     type = NULL) {
  pg <- paper_sizes

  if (!is.null(name) && !(tolower(name) %in% tolower(pg$name))) {
    name <- rlang::arg_match(
      name,
      values = as.character(pg$name),
      multiple = TRUE
    )
  }

  pg <- page_filter(pg, name, "name")

  pg <- page_filter(pg, width, "width")

  pg <- page_filter(pg, height, "height")

  if (!is.null(type) && !(tolower(type) %in% tolower(pg$type))) {
    type <- rlang::arg_match(
      type,
      values = as.character(pg$type),
      multiple = TRUE
    )
  }

  pg <- page_filter(pg, type, "type")

  if (!is.null(units)) {
    pg <- convert_page_units(pg, units)
  }

  if (is.null(orientation)) {
    return(pg)
  }

  if (!reorient) {
    orientation <- tolower(orientation)

    orientation <-
      rlang::arg_match(
        orientation,
        c("portrait", "landscape", "square"),
        multiple = TRUE
      )

    return(page_filter(pg, orientation, "orientation"))
  }

  set_page_orientation(pg, orientation)
}

#' @name get_paper
#' @rdname get_page
#' @export
get_paper <- function(name = NULL,
                      width = NULL,
                      height = NULL,
                      orientation = NULL) {
  get_page(
    name = name,
    width = width,
    height = height,
    orientation = orientation
  )
}

#' @name get_card
#' @rdname get_page
#' @export
get_card <- function(name = NULL,
                     width = NULL,
                     height = NULL,
                     orientation = NULL) {
  if (!is.null(name) && !str_detect(name, " card$")) {
    name <- paste(name, "card")
  }

  get_page(
    name = name,
    width = width,
    height = height,
    orientation = orientation,
    type = "card"
  )
}

#' @name get_page_dims
#' @rdname get_page
#' @param page Used by [get_page_dims()], page is either a character vector
#'   passed to the name parameter of [get_page()], a data.frame with column
#'   names matching the cols parameter, or a length 2 numeric vector with the
#'   page width and height.
#' @param cols Length 2 character vector with column names for page dimensions.
#'   Defaults to c("width", "height").
#' @param arg,call Passed to [cli_abort()] to improve internal error messages.
#' @param ... Additional parameters passed by [get_page_dims()] to [get_page()]
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
  if (is.character(page)) {
    page <- get_page(page, width, height, orientation, ...)
  }

  if (is.data.frame(page)) {
    check_page(page, cols[1:2], n = 1, call = call)

    return(rlang::set_names(c(page[[cols[1]]], page[[cols[2]]]), cols[1:2]))
  }

  nm <- check_dims_cols(cols, width, height)

  if (all(is.numeric(c(width, height)))) {
    return(rlang::set_names(c(width, height), cols))
  }

  if (is.numeric(page) && (length(page) == 2)) {
    return(rlang::set_names(page, cols))
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
                            height = NULL) {
  replace_cols <- c("width", "height")

  # FIXME: This should be updated to support reverse order names
  if (all(is.numeric(c(width, height))) && (cols != replace_cols)) {
    cli::cli_warn(
      c("{cols} must be {.val {replace_cols}} when
        {.arg width} and {.arg height} are provided.",
        "i" = "Replacing {.val {cols}} with {.val {replace_cols}}."
      )
    )

    return(replace_cols)
  }

  if ((length(cols) != 2) | !is.character(cols)) {
    cli::cli_warn(
      c(
        "{.arg cols} must be a length 2 {.cls character} vector.",
        "Replacing {.val {cols}} with {.val {replace_cols}}."
      )
    )

    return(replace_cols)
  }

  cols
}

#' @name convert_page_units
#' @rdname get_page
#' @param units Units to convert page dimensions to using [convert_unit_type()].
#' @export
convert_page_units <- function(page, units = NULL) {
  if (is.null(units)) {
    return(page)
  }

  if (is_same_unit_type(page$units, units)) {
    return(page)
  }

  pg_dims <-
    convert_unit_type(
      c(page$width, page$height),
      page$units,
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
page_filter <- function(page, y = NULL, col = "name") {
  if (is.null(y)) {
    return(page)
  }
  page[tolower(page[[col]]) %in% tolower(y), ]
}

#' @noRd
set_page_orientation <- function(page,
                                 orientation = NULL,
                                 tolerance = 0.1) {
  if (is.null(orientation)) {
    return(page)
  }

  pg_orientation <- has_orientation(page$width / page$height, tolerance)

  orientation <-
    match.arg(
      tolower(orientation),
      c("portrait", "landscape", "square"),
      several.ok = TRUE
    )

  switch_pg <- !(orientation == pg_orientation)

  if (!any(switch_pg)) {
    return(page)
  }

  pg_in <- page

  page[switch_pg, ] <-
    set_page_dims(
      page[switch_pg, ],
      width = page[switch_pg, ]$height,
      height = page[switch_pg, ]$width
    )

  page[switch_pg, ]$orientation <-
    has_orientation(
      page[switch_pg, ]$width / page[switch_pg, ]$height,
      tolerance
    )

  if (any(!(orientation == page$orientation))) {
    cli::cli_warn(
      c("{.arg orientation} can't be set to {.val {orientation}}
        when the page width is {.val {pg_in$width}} and height
        is {.val {pg_in$height}}.",
        "i" = "Orientation kept as {.val {pg_in$orientation}}."
      )
    )

    return(pg_in)
  }

  page
}

#' @noRd
has_orientation <- function(x, tolerance = 0.1) {
  if (length(x) > 1) {
    return(map_chr(x, has_orientation, tolerance))
  }

  if (x >= (1 + tolerance)) {
    return("landscape")
  }

  if (x <= (1 - tolerance)) {
    return("portrait")
  }

  "square"
}

#' @noRd
#' @importFrom cli cli_abort
#' @importFrom rlang caller_arg has_name
check_page <- function(page,
                       cols = c("width", "height"),
                       n = NULL,
                       arg = caller_arg(page),
                       call = parent.frame()) {
  if (!is.data.frame(page) | !all(rlang::has_name(page, cols))) {
    cli::cli_abort(
      "{.arg {arg}} must be a {.cls data.frame} with columns {.val {cols}}.",
      call = call
    )
  }

  pg_n <- nrow(page)

  if (!is.null(n) && (pg_n > n)) {
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
                          units = NULL) {
  if (!is.null(dims)) {
    stopifnot(length(dims) == 2)

    if (rlang::is_named(dims)) {
      dims <- c(dims["width"], dims["height"])
    }

    width <- dims[[1]]
    height <- dims[[2]]
  }

  page$width <- width %||% page$width
  page$height <- height %||% page$height
  page$units <- units %||% page$units

  page
}

#' Apply numeric or unit class inset to width/height dimensions of page
#'
#' @noRd
inset_page <- function(page, inset = 0.1) {
  if (is_unit(inset)) {
    dims <- get_inset_dims(c(page$width, page$height), page$units, inset)
    return(set_page_dims(page, dims))
  }

  if (length(inset) == 1) {
    inset <- rep(inset, 2)
  }

  set_page_dims(
    page,
    width = page$width * (1 - inset[[1]]),
    height = page$height * (1 - inset[[2]])
  )
}

#' Apply unit class inset to width/height dimensions
#'
#' @noRd
#' @importFrom rlang set_names
get_inset_dims <- function(dims,
                           units = "in",
                           inset = unit(1, "in"),
                           nm = c("width", "height")) {
  inset <- convert_unit_type(inset, to = units, valueOnly = TRUE)

  if (length(inset) == 1) {
    inset <- rep(inset, 2)
  }

  rlang::set_names(dims - (inset * 2), nm)
}
