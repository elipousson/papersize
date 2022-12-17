#'  Standard paper and image sizes
#'
#' Reference table of standard paper, postcard, photo print, social media image
#' sizes, and playing card sizes for [get_page_size()]. Derived from
#' [visioguy/PaperSizes](https://github.com/visioguy/PaperSizes/) repo, [Adobe
#' UK guide to photo
#' sizes](https://www.adobe.com/uk/creativecloud/photography/discover/standard-photo-sizes.html)
#' and other sources. Data is identical to data included with sfext package.
#'
#' @format A data frame with 123 rows and 9 variables:
#' \describe{
#'   \item{`name`}{Name of paper}
#'   \item{`series`}{Series}
#'   \item{`standard`}{Standard}
#'   \item{`size`}{Size in series}
#'   \item{`units`}{Units ("in", "mm", or "px") for dimensions}
#'   \item{`width`}{Width in units}
#'   \item{`height`}{Height in units}
#'   \item{`orientation`}{Portrait (width less than height), landscape, or
#'   square}
#'   \item{`type`}{Type (paper, postcard, print, or social)}
#' }
"paper_sizes"


#' Standard card sizes
#'
#' Reference table of common playing card sizes for [get_card()]. Data is a
#' subset of `paper_sizes` which is to data included with sfext package.
#'
#' @format A data frame with 5 rows and 6 variables:
#' \describe{
#'   \item{`name`}{Name of card}
#'   \item{`units`}{Units ("in" or "mm") for dimensions}
#'   \item{`width`}{Width in units}
#'   \item{`height`}{Height in units}
#'   \item{`orientation`}{Portrait (width less than height), landscape, or
#'   square}
#' }
"card_sizes"
