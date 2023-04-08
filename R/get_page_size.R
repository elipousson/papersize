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
#' @inheritParams convert_unit_type
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
#' get_page_dims(get_paper("letter"))
#'
#' convert_page_units(get_paper("letter"), units = "cm")
#'
#' @seealso [make_page_size()]
#' @seealso [get_social_size()]
#' @returns A data.frame with page, paper, or card name and dimensions.
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

  if (is.data.frame(name)) {
    check_page(name)
    return(name)
  }

  if (!is_null(name) && !(tolower(name) %in% tolower(pg$name))) {
    name <- rlang::arg_match(
      name,
      values = as.character(pg$name),
      multiple = TRUE
    )
  }

  pg <- filter_data(pg, name, ignore.case = ignore.case)

  pg <- filter_data(pg, width)

  pg <- filter_data(pg, height)

  if (!is_null(type) && !(tolower(type) %in% tolower(pg$type))) {
    type <- rlang::arg_match(
      type,
      values = as.character(pg$type),
      multiple = TRUE
    )
  }

  pg <- filter_data(pg, type, ignore.case = ignore.case)

  if (!is_null(units)) {
    pg <- convert_page_units(pg, units)
  }

  if (is_null(orientation)) {
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
  if (!is_null(name) && !str_detect(name, " card$")) {
    name <- glue("{name} card")
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
#' @importFrom rlang caller_arg set_names
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

  if (all(is_bare_numeric(c(width, height)))) {
    return(rlang::set_names(c(width, height), nm))
  }

  cli::cli_abort(
    "{.arg {arg}} must be a character with a name from {.code paper_sizes},
    a {.cls data.frame} with columns {.val {cols}},
    or a {.cls numeric} vector in the form {.code c(<width>, <height>)}.",
    call = call
  )
}

#' @noRd
#' @importFrom cli cli_warn
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

  if ((length(cols) != 2) | !is_character(cols)) {
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
#' @importFrom cliExtras cli_abort_ifnot
convert_page_units <- function(page,
                               units = NULL,
                               valueOnly = TRUE,
                               cols = c("width", "height"),
                               ...) {
  if (is_null(units)) {
    return(page)
  }

  units_col <- get_units_col()

  cli_abort_ifnot(
    "{.arg page} must have a name {.val units_col}" = has_name(page, units_col)
  )

  if (is_same_unit_type(page[[units_col]], units)) {
    return(page)
  }

  pg_dims <-
    convert_unit_type(
      c(page[[cols[1]]], page[[cols[2]]]),
      page[[units_col]],
      units,
      valueOnly = valueOnly,
      ...
    )

  set_page_dims(
    page,
    dims = pg_dims,
    units = units
  )
}


#' @noRd
#' @importFrom cli cli_abort
#' @importFrom rlang caller_arg has_name
check_page <- function(page,
                       cols = c("width", "height"),
                       n = NULL,
                       arg = caller_arg(page),
                       call = parent.frame()) {
  if ((!is.data.frame(page) & !is.list(page)) | !all(has_name(page, cols))) {
    cli::cli_abort(
      "{.arg {arg}} must be a {.cls data.frame} or {.cls list} with columns
      or names {.val {cols}}.",
      call = call
    )
  }

  pg_n <- nrow(page)

  if (is.data.frame(page) && !is_null(n) && (pg_n > n)) {
    cli::cli_abort(
      "{.arg {arg}} must have no more than {.val {n}} rows,
      and {.arg {arg}} has {.val {pg_n}}.",
      call = call
    )
  }
}
