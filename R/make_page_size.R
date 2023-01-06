#' Make a page size data frame
#'
#' Width and height or aspect ratio and either width or height are required.
#' Units are also required. If orientation is provided, width and height may be
#' reversed to match the provided orientation.
#'
#' @param width,height Page width and height. Both are required, if asp is
#'   `NULL`. Default to `NULL`.
#' @param units Units for width and height. Required. Passed to [as_unit_type()]
#'   to validate.
#' @param asp Aspect ratio. Required if only one of width or height are
#'   provided.
#' @param name Optional name for paper size. Recommend avoiding duplication with
#'   existing names in `paper_sizes`.
#' @param cols Column nmaes ot use for width and height columns.
#' @examples
#' make_page_size(48, 24, "in", name = "Tabletop map")
#'
#' make_page_size(8.5, 8.5, "in", name = "Trimmed letter")
#'
#' @export
#' @seealso [get_page_size()]
#' @importFrom rlang check_required set_names
make_page_size <- function(width = NULL,
                           height = NULL,
                           units,
                           asp = NULL,
                           orientation = NULL,
                           name = NULL,
                           cols = c("width", "height"),
                           ...) {
  check_page_asp(width, height, asp)
  rlang::check_required(units)

  width <- width %||% (height * asp)
  height <- height %||% (width / asp)
  units <- as_unit_type(units)

  pg <-
    rlang::set_names(
      data.frame(
        "width" = width,
        "height" = height,
        "units" = units
      ),
      c(cols, get_units_col())
    )

  if (!is.null(name)) {
    pg <-
      cbind(
        data.frame("name" = name),
        pg
      )
  }

  pg <- set_page_orientation(pg, orientation = orientation)

  set_page_asp(pg)
}


#' @noRd
#' @importFrom cliExtras cli_abort_if
check_page_asp <- function(width = NULL,
                           height = NULL,
                           asp = NULL,
                           call = parent.frame()) {
  cliExtras::cli_abort_if(
    "{.arg width} or {.arg height} must be provided.",
    condition = is.null(width) & is.null(height),
    call = call
  )

  cliExtras::cli_abort_if(
    "{.arg asp} must be provided if either {.arg width} or {.arg height} are {.code NULL}.",
    condition = is.null(asp) & (is.null(width) | is.null(height)),
    call = call
  )
}
