% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_contact_sheets.R
\name{make_contact_sheets}
\alias{make_contact_sheets}
\title{Use \code{magick::image_ggplot()} to make contact sheets for images}
\usage{
make_contact_sheets(
  images,
  dims = NULL,
  ncol = NULL,
  nrow = NULL,
  captions = "{file_name}\\n{date_created}",
  caption_size = 12,
  caption_position = "panel",
  image_margin = margins(0.1, unit = "in"),
  page = "letter",
  orientation = "portrait",
  image_max = NULL,
  image_fileext = NULL,
  tags = NULL,
  tz = NULL,
  save = FALSE,
  filename = NULL,
  ...
)
}
\arguments{
\item{images}{File path or data.frame from \code{\link[filenamr:read_exif]{filenamr::read_exif()}}}

\item{dims}{Image dimensions in same dimensions as page. Required.}

\item{ncol, nrow}{The dimensions of the grid to create.  If both are \code{NULL},
dims will be used or dims will be determined based on the plot dimensions.}

\item{captions}{Template for caption, passed to \code{\link[glue:glue]{glue::glue_data()}} using the
images data.frame as .x. Note that this template may vary if you are using
a custom tags parameter or modify the "filenamr.exif_xwalk" option. See
\code{\link[filenamr:read_exif]{filenamr::read_exif()}} for more details. Default:
"{file_name}\\n{date_created}"}

\item{caption_size}{Caption size, passed to \code{\link[ggplot2:element]{ggplot2::element_text()}} for
plot.caption for theme, Default: 12}

\item{caption_position}{Caption position, passed to plot.caption.position for
theme, Default: 'panel'}

\item{image_margin}{Image margin passed Default: \code{margins(0.1, unit = "in")}}

\item{page}{Paper name or a data.frame with width and height columns.
Optional if width and height are both provided, Default: \code{NULL}}

\item{orientation}{Paper orientation, Optional if width and height are both
provided, Default: 'landscape'}

\item{image_max}{Maximum number of images to use for contact sheets.}

\item{image_fileext}{Passed to fileext parameter of \code{\link[filenamr:read_exif]{filenamr::read_exif()}},
Default: \code{NULL}}

\item{tags}{List of EXIF tags to read from files. If
\code{NULL} (default), set to option "filenamr.exif_tags" or default
\code{default_exif_tags}.}

\item{tz}{Time zone to pass to \code{\link[lubridate:ymd_hms]{lubridate::ymd_hms()}} if format_exif is
\code{TRUE}. Typically set to \code{Sys.timezone()} to convert date/time columns.}

\item{save}{If \code{TRUE}, save contact sheet to a file. filename may be required
if save is \code{TRUE}. Default: \code{FALSE}}

\item{filename}{File name to create on disk.}

\item{...}{Additional parameters passed to \code{\link[=map_ggsave_ext]{map_ggsave_ext()}} excluding
width, height, and units.}
}
\value{
A list of patchwork object or (if save = TRUE) invisibly return the
list and save a file.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
Wraps \code{\link[filenamr:read_exif]{filenamr::read_exif()}}, \code{\link[magick:image_ggplot]{magick::image_ggplot()}}, and \code{\link[=page_layout]{page_layout()}}
to create contact shets for a folder of images.
}
\seealso{
\code{\link[filenamr:read_exif]{filenamr::read_exif()}}
\code{\link[glue:glue]{glue::glue()}}
\code{\link[magick:editing]{magick::editing()}}, \code{\link[magick:image_ggplot]{magick::image_ggplot()}}
\code{\link[ggplot2:labs]{ggplot2::labs()}}, \code{\link[ggplot2:theme]{ggplot2::theme()}}, \code{\link[ggplot2:element]{ggplot2::margin()}}
}
