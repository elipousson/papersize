# Standard paper and image sizes

Reference table of standard paper, postcard, photo print, social media
image sizes, and playing card sizes for
[`get_page_size()`](https://elipousson.github.io/papersize/reference/get_page_size.md).
Derived from
[visioguy/PaperSizes](https://github.com/visioguy/PaperSizes/) repo,
[Adobe UK guide to photo
sizes](https://www.adobe.com/uk/creativecloud/photography/discover/standard-photo-sizes.html)
and other sources. Data is identical to data included with `{sfext}`
package.

## Usage

``` r
paper_sizes
```

## Format

A data frame with 125 rows and 9 variables:

- `name`:

  Name of paper

- `series`:

  Series

- `standard`:

  Standard

- `size`:

  Size in series

- `units`:

  Units ("in", "mm", or "px") for dimensions

- `width`:

  Width in units

- `height`:

  Height in units

- `orientation`:

  Portrait (width less than height), landscape, or square

- `type`:

  Type (paper, postcard, print, or social)
