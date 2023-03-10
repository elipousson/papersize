% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_scale.R
\name{get_scale}
\alias{get_scale}
\alias{get_standard_scale}
\title{Get standard scales and convert to scale distances}
\usage{
get_scale(scale = NULL, standard = NULL, series = NULL)
}
\arguments{
\item{scale}{Scale name from \code{standard_scales[["scale"]]}.}

\item{standard}{Scale standard. Options include "USGS", "Engineering", or
"Architectural".}

\item{series}{Map series from \code{standard_scales[["series"]]}. Series is only
available for USGS scales.}
}
\value{
A tibble based on \link{standard_scales} with rows filtered to values that
match parameters.
}
\description{
This function returns a scale from \link{standard_scales} based on a provided
name, standard, and/or series.
}
