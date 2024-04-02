#' Make a page size data frame
#'
#' Width and height or aspect ratio and either width or height are required.
#' Units are also required. If orientation is provided, width and height may be
#' reversed to match the provided orientation.
#'
#' @param width,height Page width and height. Both are required, if asp is
#'   `NULL`. Default to `NULL`.
#' @param units Units for width and height. Required unless units is included in
#'   dims. Passed to [as_unit_type()] to validate.
#' @param asp Aspect ratio. Required if only one of width or height are
#'   provided.
#' @param name Optional name for paper size. Recommend avoiding duplication with
#'   existing names in `paper_sizes`.
#' @param cols Column names to use for width and height columns. Defaults to
#'   c("width", "height"). Must be length 2 and the first value is always used
#'   as as the width name and the second as the height.
#' @param dims A list or data.frame that can be used to set width, height,
#'   units, and/or asp.
#' @param orientation Page orientation, Default: `NULL`. Supported options are
#'   "portrait", "landscape", or "square". If width and height suggest a
#'   portrait orientation when orientation = "landscape", the dimensions are
#'   reversed so the page dimensions match the provided orientation.
#' @inheritParams as_unit_type
#' @param class Class of return object: "data.frame" (default) or "list" (only
#'   supported when input page size is a single row).
#' @examples
#' make_page_size(48, 24, "in", name = "Tabletop map")
#'
#' make_page_size(8.5, 8.5, "in", name = "Trimmed letter")
#'
#' make_page_size(5, asp = 1.25, units = "cm", class = "list")
#'
#' @returns A data.frame with columns named (by default) width, height, units,
#'   orientation, and asp or a list with those same names.
#' @seealso [get_page_size()]
#' @export
#' @importFrom rlang check_required set_names arg_match
make_page_size <- function(width = NULL,
                           height = NULL,
                           units,
                           asp = NULL,
                           orientation = NULL,
                           name = NULL,
                           dims = NULL,
                           valid_units = NULL,
                           cols = c("width", "height"),
                           class = "data.frame",
                           call = caller_env()) {
  if (is_named(dims)) {
    if (has_name(dims, cols[1])) {
      width <- width %||% as.numeric(dims[[cols[1]]])
    }

    if (has_name(dims, cols[2])) {
      height <- height %||% as.numeric(dims[[cols[2]]])
    }

    if (has_name(dims, get_asp_col())) {
      asp <- asp %||% as.numeric(dims[[get_asp_col()]])
    }

    if (missing(units) && has_name(dims, get_units_col())) {
      units <- dims[[get_units_col()]]
    }
  }

  check_page_asp(width = width, height = height, asp = asp, call = call)
  rlang::check_required(units)

  cli_if(
    x = !all(is.numeric(c(width, height, asp))),
    "{.arg width}, {.arg height}, and {.arg asp} must all be either
    {.cls numeric} or {.code NULL}.",
    .fn = cli::cli_abort
  )

  width <- width %||% (height * asp)
  height <- height %||% (width / asp)
  units <- as_unit_type(units, valid_units = valid_units)

  pg <- set_names(
    data.frame(
      "width" = width,
      "height" = height,
      "units" = units
    ),
    c(cols, get_units_col())
  )

  if (!is_null(name)) {
    pg <- cbind(
      data.frame("name" = name),
      pg
    )
  }

  pg <- set_page_orientation(pg, cols = cols, orientation = orientation)

  pg <- set_page_asp(pg, cols = cols)

  class <- arg_match(class, c("data.frame", "list"))

  if (class == "list") {
    return(page_to_list(pg))
  }

  pg
}

#' Convert page data.frame to a named list
#'
#' @noRd
page_to_list <- function(x) {
  stopifnot(
    is.data.frame(x),
    nrow(x) == 1
  )

  set_names(
    lapply(
      names(x),
      function(nm) {
        x[[nm]]
      }
    ),
    names(x)
  )
}

#' Check if correct input args are provided
#'
#' @noRd
check_page_asp <- function(width = NULL,
                           height = NULL,
                           asp = NULL,
                           call = caller_env()) {
  cli_if(
    x = is_null(width) && is_null(height),
    "{.arg width} or {.arg height} must be provided.",
    .fn = cli::cli_abort,
    call = call
  )

  cli_if(
    x = is_null(asp) && (is_null(width) || is_null(height)),
    "{.arg asp} must be provided if only {.arg width} or only
    {.arg height} are provided.",
    .fn = cli::cli_abort,
    call = call
  )
}
