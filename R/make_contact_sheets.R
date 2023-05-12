#' Use `magick::image_ggplot()` to make contact sheets for images
#'
#' Wraps [filenamr::read_exif()], [magick::image_ggplot()], and [page_layout()]
#' to create contact shets for a folder of images.
#'
#' @param images File path or data.frame from [filenamr::read_exif()]
#' @param dims Image dimensions in same dimensions as page. Required.
#' @param image_fileext Passed to fileext parameter of [filenamr::read_exif()],
#'   Default: `NULL`
#' @inheritParams filenamr::read_exif
#' @param captions Template for caption, passed to [glue::glue()] for execution
#'   within function environment. Default: '{images$file_name}
#'   '{images$create_date}'
#' @param caption_size Caption size, passed to [ggplot2::element_text()] for plot.caption
#'   for theme, Default: 12
#' @param caption_position Caption position, passed to plot.caption.position for
#'   theme, Default: 'panel'
#' @param image_margin Image margin passed Default: `margins(0.1, unit = "in")`
#' @inheritParams page_layout
#' @inheritParams get_page_size
#' @param save If `TRUE`, save contact sheet to a file. filename may be required
#'   if save is `TRUE`. Default: `FALSE`
#' @inheritParams map_ggsave_ext
#' @param ... Additional parameters passed to [map_ggsave_ext()] excluding
#'   width, height, and units.
#' @return A list of patchwork object or (if save = TRUE) invisibly return the
#'   list and save a file.
#' @seealso
#'  [filenamr::read_exif()]
#'  [glue::glue()]
#'  [magick::editing()], [magick::image_ggplot()]
#'  [ggplot2::labs()], [ggplot2::theme()], [ggplot2::margin()]
#' @rdname make_contact_sheets
#' @export
#' @importFrom glue glue
#' @importFrom cli cli_progress_step
make_contact_sheets <- function(images,
                                dims,
                                image_fileext = NULL,
                                tags = NULL,
                                captions = "file_name",
                                caption_size = 12,
                                caption_position = "panel",
                                image_margin = margins(0.1, unit = "in"),
                                page = "letter",
                                orientation = "portrait",
                                save = FALSE,
                                filename = NULL,
                                ...) {
  if (is.character(images)) {
    check_installed("filenamr")
    cli::cli_progress_step("Reading image EXIF data")
    images <- filenamr::read_exif(path = images, fileext = image_fileext, tags = tags)
  }

  if (is_string(captions) && has_name(images, captions)) {
    captions <- images[[captions]]
  }

  # stopifnot(all(has_name(images, c("img_width", "img_height"))))
  # dims <- dims %||% ((max(images[["img_width"]]) / min(images[["img_height"]])) / dpi)

  check_installed(c("ggplot2", "magick"))

  page <- get_page_size(page, orientation = orientation)

  magick_images <- map(
    images[["path"]],
    function(x) {
      magick::image_read(x)
    }
  )

  image_theme <-
    ggplot2::theme(
      plot.caption = ggplot2::element_text(size = caption_size),
      plot.caption.position = caption_position,
      plot.margin = image_margin
    )

  cli::cli_progress_step("Creating plots from images")

  plots <-
    map2(
      magick_images,
      captions,
      function(x, y) {
        x <- magick::image_ggplot(x)

        x +
          ggplot2::labs(
            caption = y
          ) +
          image_theme
      }
    )

  cli::cli_progress_step("Creating contact sheets plots")

  sheets <-
    page_layout(
      plots = plots,
      page = page,
      dims = dims,
      images = TRUE
    )

  if (!save) {
    return(sheets)
  }

  cli::cli_progress_step("Saving contact sheets to file")

  map_ggsave_ext(
    sheets,
    paper = page,
    filename = filename,
    ...
  )
}
