
#' Helper functions for grid units
#'
#' @description
#' - [as_unit()]: Convert to a unit (allowing unit objects as units)
#' - [as_unit_type()]: Convert to unit type (or checking unit types)
#' - [convert_unit_type()]: Convert x from one unit type to another (preserving
#' names for named vectors)
#' - [is_unit_type()]: Is x a character vector with a unit type supported by the
#' grid package or a unit object with a supported type?
#' - [is_same_unit_type()]: Are x and y the same unit type?
#'
#' Note, when [as_unit_type()] is used on a margin object, it returns the unique
#' unit type as a length 1 character vector not a length 4 character vector as
#' you could expect with other length 4 input objects.
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
#' is_unit_type("inch")
#'
#' is_unit_type("inchs")
#'
#' is_same_unit_type(inch, "in")
#'
#' is_same_unit_type("pt", "points")
#'
#' @inheritParams grid::unit
#' @param arg Passed to [cli_abort()] to improve internal error messages.
#' @inheritParams cli::cli_abort
#' @returns A `unit` class object, a character vector with a unit type, or a
#'   logical vector.
#' @export
#' @importFrom cliExtras cli_abort_if
#' @importFrom rlang try_fetch
#' @importFrom grid unit
#' @importFrom cli cli_abort
as_unit <- function(x,
                    units = NULL,
                    data = NULL,
                    recurse = FALSE,
                    arg = caller_arg(x),
                    call = parent.frame()) {
  num <- suppressWarnings(as.numeric(x))

  cliExtras::cli_abort_if(
    "{.arg {arg}} must be {.cls numeric} or an object that can be coerced to
    {.cls numeric}.",
    condition = any(is.na(num))
  )

  if (is.null(units)) {
    units <- x
  }

  units <- as_unit_type(units, recurse, call = call)

  rlang::try_fetch(
    grid::unit(num, units, data),
    error = function(cnd) {
      cli::cli_abort(
        "{.arg {arg}} can't be coerced to a unit object of type
        {.val {units}}.",
        call = call, parent = cnd
      )
    }
  )
}

#' @name as_unit_type
#' @rdname as_unit
#' @inheritParams grid::unitType
#' @inheritParams grid::unit
#' @export
#' @importFrom rlang check_installed try_fetch
#' @importFrom grid unitType unit
#' @importFrom cli cli_abort
as_unit_type <- function(x,
                         recurse = FALSE,
                         data = NULL,
                         arg = caller_arg(x),
                         call = parent.frame()) {
  if (is_unit(x)) {
    type <- grid::unitType(x, recurse)

    if (is_margin(x)) {
      return(unique(type))
    }

    return(type)
  }

  rlang::try_fetch(
    grid::unitType(grid::unit(1, x, data), recurse),
    error = function(cnd) {
      cli::cli_abort(
        "{.arg {arg}} ({.val {x}}) can't be coerced to a grid unit type.",
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
#' @importFrom rlang check_installed is_named set_names
#' @importFrom cli cli_warn
#' @importFrom grid convertUnit
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

    if (!isTRUE(valueOnly)) {
      x <- as_unit(x, to)
    }

    return(rlang::set_names(x, nm))
  }

  if (is_unit(x) && is_unit_type(from) && !is_same_unit_type(x, from)) {
    cli::cli_warn(
      "Existing {.arg x} unit type {.val {as_unit_type(x)}} is ignored
        when {.arg from} is provided."
    )
  }

  x <- as_unit(x, from)

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

#' @name is_unit_type
#' @rdname as_unit
#' @export
is_unit_type <- function(x, ...) {
  if (is.null(x)) {
    return(FALSE)
  }

  if (all(x %in% grid_units[1:27])) {
    return(TRUE)
  }

  rlang::try_fetch(
    is_unit(as_unit(1, units = x, ..., arg = "x")),
    error = function(cnd) {
      FALSE
    }
  )
}

#' @name is_same_unit_type
#' @rdname as_unit
#' @param y Object to compare to x.
#' @export
is_same_unit_type <- function(x, y, recurse = FALSE, data = NULL) {
  as_unit_type(x, recurse, data) == as_unit_type(y, recurse, data)
}
