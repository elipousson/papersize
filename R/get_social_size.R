#' Get social media image size to match platform and format
#'
#' See `paper_sizes[paper_sizes$type == "social",]$name` for support image
#' options.
#'
#' @param name Image size name, Default: `NULL`
#' @param platform Social media platform, "Instagram", "Facebook", or "Twitter",
#'   Default: `NULL`
#' @param format Image format, "post", "story", or "cover", Default: `NULL`
#' @param orientation Image orientation, Default: `NULL`.
#' @examples
#' get_social_size("Instagram post")
#'
#' get_social_size(platform = "Twitter")
#'
#' get_social_size(format = "cover")
#'
#' @seealso [get_page_size()]
#' @export
get_social_size <- function(name = NULL, platform = NULL, format = NULL, orientation = NULL) {
  image_sizes <- get_page_size(type = "social")

  if (!is_null(platform)) {
    platform <- tolower(platform)
    platform <- arg_match(platform, tolower(unique(image_sizes$standard)))
    image_sizes <- image_sizes[tolower(image_sizes$standard) %in% platform, ]
  }

  if (!is_null(format)) {
    format <- tolower(format)
    format <- arg_match(format, tolower(unique(image_sizes$size)))
    image_sizes <- image_sizes[tolower(image_sizes$size) %in% format, ]
  }

  if (!is_null(name)) {
    name <- tolower(name)
    name_opts <- tolower(as.character(image_sizes$name))
    name <- arg_match(name, name_opts)

    image_sizes <-
      filter_data(
        image_sizes,
        name
      )
  }

  set_page_orientation(
    image_sizes,
    orientation = orientation
  )
}
