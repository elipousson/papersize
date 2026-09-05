# Annotate a plot with a wavy (sine) line

`annotate_wave()` draws a wavy line as a ggplot2 annotation layer, built
with
[`ggplot2::annotate()`](https://ggplot2.tidyverse.org/reference/annotate.html)
and
[`ggplot2::geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html).
Useful for decorative dividers, underlines, or water/wave motifs.

## Usage

``` r
annotate_wave(
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
)
```

## Arguments

- xmin, xmax:

  Horizontal range the wave spans.

- y:

  Baseline (vertical center) the wave oscillates around, Default: 0

- amplitude:

  Height of the wave above and below `y`, Default: 1

- period:

  Horizontal distance the wave takes to complete one full cycle,
  Default: 2

- phase:

  Horizontal shift of the wave, in radians, Default: 0

- n:

  Number of points used to draw the curve. Higher values produce a
  smoother `"wave"` curve. Ignored for `"zigzag"`, Default: 200

- color, linewidth, lineend:

  Aesthetics passed to
  [`ggplot2::geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html),
  Default: `"black"`, `1`, and `"round"`

- linestyle:

  Either `"wave"` for a smooth sine curve or `"zigzag"` for an angular
  zigzag (triangle wave) between the same peaks and troughs, Default:
  `"wave"`

- ...:

  Additional arguments passed to
  [`ggplot2::annotate()`](https://ggplot2.tidyverse.org/reference/annotate.html).

## Value

A ggplot2 annotation layer that can be added to a ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
if (interactive() && is_installed("ggplot2")) {
  ggplot2::ggplot() +
    annotate_wave(xmin = 0, xmax = 10, amplitude = 0.5, period = 2.2) +
    annotate_wave(
      xmin = 0,
      xmax = 10,
      y = -2,
      amplitude = 0.5,
      period = 2.2,
      linestyle = "wiggle"
    )
}
} # }
```
