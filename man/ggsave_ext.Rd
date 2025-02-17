% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggsave_ext.R
\name{ggsave_ext}
\alias{ggsave_ext}
\alias{ggsave_social}
\alias{map_ggsave_ext}
\title{Save a ggplot2 plot to file and update file EXIF metadata}
\usage{
ggsave_ext(
  plot = ggplot2::last_plot(),
  name = NULL,
  label = NULL,
  prefix = NULL,
  postfix = NULL,
  increment = NULL,
  filename = NULL,
  device = NULL,
  fileext = NULL,
  filetype = NULL,
  path = NULL,
  paper = NULL,
  orientation = NULL,
  width = NULL,
  height = NULL,
  asp = NULL,
  units = getOption("papersize.ggsave_units", "in"),
  scale = 1,
  dpi = 300,
  bgcolor = NULL,
  exif = FALSE,
  title = NULL,
  author = NULL,
  keywords = NULL,
  args = NULL,
  overwrite = TRUE,
  ask = FALSE,
  preview = FALSE,
  limitsize = TRUE,
  quiet = FALSE,
  ...
)

ggsave_social(
  plot = ggplot2::last_plot(),
  image = "Instagram post",
  platform = NULL,
  format = NULL,
  orientation = NULL,
  name = NULL,
  filename = NULL,
  fileext = "jpeg",
  filetype = NULL,
  dpi = 72,
  width = 1080,
  height = 1080,
  units = "px",
  ...
)

map_ggsave_ext(
  plot,
  name = NULL,
  label = NULL,
  prefix = NULL,
  postfix = "pg_",
  filename = NULL,
  device = NULL,
  fileext = NULL,
  filetype = NULL,
  path = NULL,
  overwrite = TRUE,
  ...,
  single_file = TRUE,
  onefile = TRUE
)
}
\arguments{
\item{plot}{Plot to save, defaults to last plot displayed. If plot is an
"magick-image" class object, it is converted to a plot using
\code{\link[magick:image_ggplot]{magick::image_ggplot()}}}

\item{name}{Plot name, used to create filename (if filename is \code{NULL}) using
\code{\link[filenamr:make_filename]{filenamr::make_filename()}}}

\item{label}{Label to combine with name converted to snake case with
\code{\link[janitor:make_clean_names]{janitor::make_clean_names()}}. The label is designed to identify the area
or other shared characteristics across multiple data files, maps, or plots.
label is ignored if name is NULL or if name includes a file extension.}

\item{prefix}{File name prefix. "date" adds a date prefix, "time" adds a
date/time prefix; defaults to \code{NULL}.}

\item{postfix}{File name postfix; defaults to \code{NULL}.}

\item{increment}{If \code{TRUE}, increment digits in string by 1. If numeric,
increment digits in string by value. If \code{NULL}, 0, or if no digits are
present in string, return string as is.}

\item{filename}{File name to create on disk.}

\item{device}{Device to use. Can either be a device function
(e.g. \link{png}), or one of "eps", "ps", "tex" (pictex),
"pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf" (windows only). If
\code{NULL} (default), the device is guessed based on the \code{filename} extension.}

\item{fileext}{File type or extension. Optional if filename or path include a
file extension.}

\item{filetype}{File type (used if fileext is \code{NULL}).}

\item{path}{Path of the directory to save plot to: \code{path} and \code{filename}
are combined to create the fully qualified file name. Defaults to the
working directory.}

\item{paper}{Paper matching name from \code{paper_sizes} (e.g. "letter"). Not case
sensitive.}

\item{orientation}{Page orientation ("portrait", "landscape", or "square").}

\item{width, height}{Plot size in units expressed by the \code{units} argument.
If not supplied, uses the size of the current graphics device.}

\item{asp}{Numeric aspect ratio used to determine width or height if only one
of the two arguments is provided; defaults to \code{NULL}.}

\item{units}{One of the following units in which the \code{width} and \code{height}
arguments are expressed: \code{"in"}, \code{"cm"}, \code{"mm"} or \code{"px"}.}

\item{scale}{Multiplicative scaling factor.}

\item{dpi}{Plot resolution. Also accepts a string input: "retina" (320),
"print" (300), or "screen" (72). Applies only to raster output types.}

\item{bgcolor}{Background color to optionally override \code{plot.background}
theme element.}

\item{exif}{If \code{TRUE}, the EXIF metadata for the exported file is updated
with the exifr package; defaults to \code{FALSE}.}

\item{title}{Title to add to file metadata with exiftoolr, Default: \code{NULL}.}

\item{author}{Author to add to file metadata to the "Author" and
"XMP-dc:creator" tags. Default: \code{NULL}.}

\item{keywords}{Keyword(s) added to file metadata to "IPTC:Keywords" and
"XMP-dc:Subject" tags. Defaults to \code{NULL}.}

\item{args}{Alternate arguments passed to \code{\link[exiftoolr:exif_call]{exiftoolr::exif_call()}}. Other tag
parameters are appended to args if they are not \code{NULL}.}

\item{overwrite}{If \code{TRUE} (default), overwrite any existing file with the
same name or ask to overwrite if \code{ask = TRUE}. Passed to
\code{\link[filenamr:check_file_overwrite]{filenamr::check_file_overwrite()}}.}

\item{ask}{If \code{TRUE}, ask before overwriting file with the same name.
Defaults to \code{FALSE}. Passed to \code{\link[filenamr:check_file_overwrite]{filenamr::check_file_overwrite()}}.}

\item{preview}{If \code{TRUE}, open saved file in default system application.
Based on ggpreview from tjmisc package.}

\item{limitsize}{When \code{TRUE} (the default), \code{ggsave()} will not
save images larger than 50x50 inches, to prevent the common error of
specifying dimensions in pixels.}

\item{quiet}{If \code{TRUE} (default), suppress function messages.}

\item{...}{
  Arguments passed on to \code{\link[ggplot2:ggsave]{ggplot2::ggsave}}
  \describe{
    \item{\code{create.dir}}{Whether to create new directories if a non-existing
directory is specified in the \code{filename} or \code{path} (\code{TRUE}) or return an
error (\code{FALSE}, default). If \code{FALSE} and run in an interactive session,
a prompt will appear asking to create a new directory when necessary.}
  }}

\item{image}{Image name passed to name parameter of \code{\link[=get_social_size]{get_social_size()}}.}

\item{platform}{Social media platform, "Instagram", "Facebook", or "Twitter",
Default: \code{NULL}}

\item{format}{Image format, "post", "story", or "cover", Default: \code{NULL}}

\item{single_file, onefile}{If \code{TRUE}, use \code{\link[gridExtra:arrangeGrob]{gridExtra::arrangeGrob()}} to create an
arrangelist class object that \code{\link[ggplot2:ggsave]{ggplot2::ggsave()}} can save as a single
multi-page file. Note: this does not work with plots modified with
patchwork including inset maps created with the
\code{\link[maplayer:layer_inset]{maplayer::layer_inset()}} function.}
}
\description{
Save a plot or map then update the EXIF metadata for the title, author, and
create data. \code{\link[=ggsave_ext]{ggsave_ext()}} also supports creating a file name based on a
sentence case name with spaces (e.g. "Baltimore city map") and appending a
label (e.g. "baltcity") as a prefix to the output file name.

\code{\link[=map_ggsave_ext]{map_ggsave_ext()}} can take a list of \{ggplot2\} plots (e.g. a list of plot
generated by \code{\link[purrr:map]{purrr::map()}}) and create multiple files using the same
parameters or use the \{gridExtras\} package to create a single combined PDF
file.
}
\seealso{
\code{\link[ggplot2:ggsave]{ggplot2::ggsave()}}
}
