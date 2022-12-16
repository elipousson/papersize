#' Get a paper or card size based on name, dimensions, orientation, or type
#'
#' @name get_page
#' @param name Page name, e.g. "letter", not case sensitive, Default: NULL
#' @param width Page width in "in", "px" or "mm" units. Default: NULL
#' @param height Page height in "in", "px" or "mm" units. Default: NULL
#' @param orientation Page orientation, Default: NULL
#' @param type Page type, Options include "paper", "social", "postcard",
#'   "print", "card", or "screen". Default: NULL
#' @return A data.frame with page, paper, or card name and dimensions.
#' @export
get_page <- function(name = NULL,
                     width = NULL,
                     height = NULL,
                     orientation = NULL,
                     type = NULL) {
  page <- paper_sizes

  page <- page_filter(page, name, "name")

  page <- page_filter(page, width, "width")

  page <- page_filter(page, height, "height")

  page <- page_filter(page, type, "type")

  page_orientation(page, orientation)
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

#' @noRd
page_filter <- function(page, y = NULL, col = "name") {
  if (is.null(y)) {
    return(page)
  }
  page[tolower(page[[col]]) %in% tolower(y), ]
}

#' @noRd
page_orientation <- function(page, orientation = NULL, tolerance = 0.1) {
  if (is.null(orientation)) {
    return(page)
  }

  orientation <-
    match.arg(
      tolower(orientation),
      c("portrait", "landscape", "square"),
      several.ok = TRUE
    )

  if (all(page$orientation %in% orientation)) {
    return(page)
  }

  width <- page$width
  height <- page$height
  page$width <- height
  page$height <- width

  page$orientation <- has_orientation(page$width / page$height, tolerance)

  page_filter(page, orientation, "orientation")
}

#' @noRd
has_orientation <- function(x, tolerance = 0.1) {
  if (length(x) > 1) {
    return(map_chr(x, ~ is_orientation(.x, tolerance)))
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
                       arg = caller_arg(page),
                       cols = c("width", "height"),
                       call = parent.frame()) {
  if (!is.data.frame(page) | !all(rlang::has_name(page, cols))) {
    cli::cli_abort(
      "{.arg {arg}} must be a {.cls data.frame} with columns {.val {cols}}.",
      call = call
    )
  }
}

#' @noRd
#' @importFrom rlang caller_arg
#' @importFrom cli cli_abort
page_dims <- function(page = NULL,
                      width = NULL,
                      height = NULL,
                      orientation = NULL,
                      arg = caller_arg(page),
                      call = parent.frame(),
                      ...) {
  if (is.character(page)) {
    page <- get_page(page, width, height, orientation, ...)
  }

  if (is.data.frame(page)) {
    check_page(page, call = call)

    return(c(page$width, page$height))
  }

  if (all(is.numeric(c(width, height)))) {
    return(c(width, height))
  }

  if (is.numeric(page) && (length(page) == 2)) {
    return(page)
  }

  cols <- c("width", "height")

  cli::cli_abort(
    "{.arg {arg}} must be a character with a name from {.code paper_sizes},
    a {.cls data.frame} with columns {.val {cols}},
    or a {.cls numeric} vector in the form {.code c(width, height)}.",
    call = call
    )
}

#' @noRd
inset_page <- function(page, inset = 0.1) {
  if (is_unit(inset)) {
    return(unit_page_inset(page, inset))
  }

  if (length(inset) == 1) {
    inset <- rep(inset, 2)
  }

  page$width <- page$width * (1 - inset[[1]])
  page$height <- page$height * (1 - inset[[1]])

  page
}

#' @noRd
unit_page_inset <- function(page, inset = unit(1, "in")) {
  if (all(gg_is_same_unit(page$units, inset))) {
    inset <- as.numeric(inset)
  } else {
    inset <- gg_convert_unit(inset, to = page$units, value_only = TRUE)
  }

  if (length(inset) == 1) {
    inset <- rep(inset, 2)
  }

  page$width <- page$width - (inset[[1]] * 2)
  page$height <- page$height - (inset[[2]] * 2)

  page
}
