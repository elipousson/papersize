# Get social media image size to match platform and format

See `paper_sizes[paper_sizes$type == "social",]$name` for support image
options.

## Usage

``` r
get_social_size(
  name = NULL,
  platform = NULL,
  format = NULL,
  orientation = NULL
)
```

## Arguments

- name:

  Image size name, Default: `NULL`

- platform:

  Social media platform, "Instagram", "Facebook", or "Twitter", Default:
  `NULL`

- format:

  Image format, "post", "story", or "cover", Default: `NULL`

- orientation:

  Image orientation, Default: `NULL`.

## See also

[`get_page_size()`](https://elipousson.github.io/papersize/reference/get_page_size.md)

## Examples

``` r
get_social_size("Instagram post")
#> # A tibble: 1 × 10
#>   name          series size  standard units width height orientation type    asp
#>   <chr>         <chr>  <chr> <chr>    <chr> <dbl>  <dbl> <chr>       <chr> <dbl>
#> 1 Instagram po… NA     post  Instagr… px     1080   1080 square      soci…     1

get_social_size(platform = "Twitter")
#> # A tibble: 4 × 10
#>   name          series size  standard units width height orientation type    asp
#>   <chr>         <chr>  <chr> <chr>    <chr> <dbl>  <dbl> <chr>       <chr> <dbl>
#> 1 Twitter cove… NA     cover Twitter  px     1500    500 landscape   soci… 3    
#> 2 Twitter imag… NA     post  Twitter  px     1200    628 landscape   soci… 1.91 
#> 3 Twitter sing… NA     post  Twitter  px     1200    675 landscape   soci… 1.78 
#> 4 Twitter mult… NA     post  Twitter  px      700    800 portrait    soci… 0.875

get_social_size(format = "cover")
#> # A tibble: 2 × 10
#>   name          series size  standard units width height orientation type    asp
#>   <chr>         <chr>  <chr> <chr>    <chr> <dbl>  <dbl> <chr>       <chr> <dbl>
#> 1 Facebook cov… NA     cover Facebook px      820    312 landscape   soci…  2.63
#> 2 Twitter cove… NA     cover Twitter  px     1500    500 landscape   soci…  3   
```
