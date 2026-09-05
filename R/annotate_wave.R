#' Annotate a plot with a wavy (sine) line
#'
#' [annotate_wave()] draws a wavy line as a ggplot2 annotation layer, built
#' with [ggplot2::annotate()] and [ggplot2::geom_line()]. Useful for
#' decorative dividers, underlines, or water/wave motifs.
#'
#' @param xmin,xmax Horizontal range the wave spans.
#' @param y Baseline (vertical center) the wave oscillates around, Default: 0
#' @param amplitude Height of the wave above and below `y`, Default: 1
#' @param period Horizontal distance the wave takes to complete one full
#'   cycle, Default: 2
#' @param phase Horizontal shift of the wave, in radians, Default: 0
#' @param n Number of points used to draw the curve. Higher values produce a
#'   smoother `"wave"` curve. Ignored for `"zigzag"`, Default: 200
#' @param color,linewidth,lineend Aesthetics passed to [ggplot2::geom_line()],
#'   Default: `"black"`, `1`, and `"round"`
#' @param linestyle Either `"wave"` for a smooth sine curve or `"zigzag"` for
#'   an angular zigzag (triangle wave) between the same peaks and troughs,
#'   Default: `"wave"`
#' @param ... Additional arguments passed to [ggplot2::annotate()].
#' @return A ggplot2 annotation layer that can be added to a ggplot object.
#' @examples
#' \dontrun{
#' if (interactive() && is_installed("ggplot2")) {
#'   ggplot2::ggplot() +
#'     annotate_wave(xmin = 0, xmax = 10, amplitude = 0.5, period = 2.2) +
#'     annotate_wave(
#'       xmin = 0,
#'       xmax = 10,
#'       y = -2,
#'       amplitude = 0.5,
#'       period = 2.2,
#'       linestyle = "wiggle"
#'     )
#' }
#' }
#' @keywords annotate
#' @export
#' @importFrom rlang check_installed arg_match
annotate_wave <- function(
  xmin,
  xmax,
  y = 0,
  amplitude = 1,
  period = 2,
  phase = 0,
  n = 200,
  color = "black",
  linewidth = 1,
  lineend = "round",
  linestyle = c("wave", "zigzag"),
  ...
) {
  check_installed("ggplot2")
  linestyle <- arg_match(linestyle)

  x <- seq(xmin, xmax, length.out = n)
  theta <- 2 * pi * (x - xmin) / period + phase

  if (linestyle == "zigzag") {
    wave_y <- y + amplitude * sin(theta)
  } else {
    # Triangle wave: same peaks/troughs as the sine curve, connected by
    # straight segments instead of a smooth curve
    wave_y <- y + (2 * amplitude / pi) * asin(sin(theta))
  }

  ggplot2::annotate(
    ggplot2::GeomLine,
    x = x,
    y = wave_y,
    color = color,
    linewidth = linewidth,
    lineend = lineend,
    ...
  )
}
