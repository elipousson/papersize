#' Apply numeric or unit class inset to width/height dimensions of page
#'
#' [inset_page()] applies a set inset distance value or percent distance to
#' modify the dimensions of the input page list or data frame.
#'
#' @param inset Numeric distance to inset an input page.
#' @param pct_inset Percent inset based on existing page dimensions.
#' @inheritParams as_unit
#' @keywords internal
inset_page <- function(page,
                       inset = 0.1,
                       units = "in",
                       pct_inset = NULL,
                       cols = c("width", "height")) {
  units_col <- get_units_col()

  if (!is_unit(inset) && is_null(pct_inset)) {
    inset <- as_unit(inset, units)
  }

  if (is_unit(inset)) {
    dims <- get_inset_dims(c(page[[cols[1]]], page[[cols[2]]]), page[[units_col]], inset)
    return(set_page_dims(page, dims, cols = cols))
  }

  if (has_length(pct_inset, 1)) {
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
#' @importFrom grid unit
#' @importFrom rlang set_names
get_inset_dims <- function(dims,
                           units = "in",
                           inset = unit(1, "in"),
                           nm = c("width", "height")) {
  inset <- convert_unit_type(inset, to = units, valueOnly = TRUE)

  if (has_length(inset, 1)) {
    inset <- rep(inset, 2)
  }

  rlang::set_names(dims - (inset * 2), nm)
}
