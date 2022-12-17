
#' Helper functions for grid units
#'
#' @description
#' - [as_unit()]: Convert to unit (allowing unit objects as units)
#' - [as_unit_type()]: Convert to unit type (or checking unit types)
#' - [convert_unit_type()]: Convert x from one unit type to another (preserving
#' names for named vectors)
#' - [is_same_unit_type()]: Are x and y the same unit type?
#'
#' @name as_unit
#' @examples
#'
#' inch <- as_unit(1, "in")
#'
#' inch
#'
#' as_unit(10, inch)
#'
#' as_unit(inch, "cm")
#'
#' as_unit(inch)
#'
#' convert_unit_type(inch, to = "cm")
#'
#' convert_unit_type(c(10, 100), from = "mm", to = "cm")
#'
#' is_same_unit_type(inch, "in")
#'
#' is_same_unit_type("pt", "points")
#'
#' @inheritParams grid::unit
#' @param arg Passed to [cli_abort()] to improve internal error messages.
#' @inheritParams cli::cli_abort
#' @export
as_unit <- function(x,
                    units = NULL,
                    data = NULL,
                    recurse = FALSE,
                    arg = caller_arg(x),
                    call = parent.frame()) {
  rlang::try_fetch(
    grid::unit(as.numeric(x), as_unit_type(units %||% x, recurse), data),
    error = function(cnd) {
      cli::cli_abort(
        "{.arg {arg}} can't be coerced to a unit object.",
        call = call,
        parent = cnd
      )
    }
  )
}

#' @name as_unit_type
#' @rdname as_unit
#' @inheritParams grid::unitType
#' @export
#' @importFrom rlang caller_arg try_fetch
#' @importFrom grid unitType unit
#' @importFrom cli cli_abort
as_unit_type <- function(x,
                         recurse = FALSE,
                         arg = caller_arg(x),
                         call = parent.frame()) {
  if (is_unit(x)) {
    return(grid::unitType(x, recurse))
  }

  rlang::try_fetch(
    grid::unitType(grid::unit(1, x), recurse),
    error = function(cnd) {
      cli::cli_abort(
        "{.arg {arg}} can't be coerced to a unit type.",
        call = call,
        parent = cnd
      )
    }
  )
}

#' @name convert_unit_type
#' @rdname as_unit
#' @param from Unit to convert from. If `NULL` and x is not a units object,
#'   convert to `to` units with a warning.
#' @param to Unit to convert to. Passed to unitTo parameter of
#'   [grid::convertUnit()]. If `NULL`, return x as is.
#' @param typeFrom Passed to typeFrom parameter of [grid::convertUnit()].
#'   Defaults to "dimension".
#' @param valueOnly Passed to valueOnly parameter of [grid::convertUnit()].
#'   Defaults to `FALSE`.
#' @inheritDotParams grid::convertUnit -unitTo
#' @export
#' @importFrom rlang is_named set_names
#' @importFrom cli cli_abort
#' @importFrom grid unit convertUnit
convert_unit_type <- function(x,
                              from = NULL,
                              to = NULL,
                              typeFrom = "dimension",
                              valueOnly = FALSE,
                              ...) {
  if (is.null(to)) {
    return(x)
  }

  nm <- NULL

  if (rlang::is_named(x)) {
    nm <- names(x)
    x <- rlang::set_names(x, NA)
  }

  if (!is_unit(x) && is.null(from)) {
    cli::cli_warn(
      "{.arg from} is {.code NULL}, converting {.arg x} to {.val {to}}."
    )
    return(rlang::set_names(as_unit(x, to), nm))
  }

  if (is_unit(x) && is_unit(from) && !is_same_unit_type(x, from)) {
    cli::cli_warn(
      "Existing  {.arg x} unit type {.val {as_unit_type(x)}} is ignored
        when {.arg from} is a {.cls unit} object."
    )
  }

  x <- as_unit(x, from)

  if (!is_unit(x)) {
    cli::cli_abort(
      "{.arg x} must be a {.cls numeric} vector or {.cls unit} object."
    )
  }

  x <-
    grid::convertUnit(
      x,
      unitTo = as_unit_type(to),
      valueOnly = valueOnly,
      typeFrom = typeFrom,
      ...
    )

  rlang::set_names(x, nm)
}

#' @name is_same_unit_type
#' @rdname as_unit
#' @param y Object to compare to x.
#' @export
is_same_unit_type <- function(x, y, recurse = FALSE) {
  as_unit_type(x, recurse) == as_unit_type(y, recurse)
}
