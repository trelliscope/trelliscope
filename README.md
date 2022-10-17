
<!-- README.md is generated from README.Rmd. Please edit that file -->

# trelliscope

<!-- badges: start -->

[![R-CMD-check](https://github.com/trelliscope/trelliscope/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/trelliscope/trelliscope/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/trelliscope/trelliscope/branch/main/graph/badge.svg)](https://app.codecov.io/gh/trelliscope/trelliscope?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

This repository contains an experimental rewrite of the trelliscopejs R
package. It is currently under heavy development.

## Installation

You can install the development version of trelliscope from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("trelliscope/trelliscope")
```

## Example

``` r
library(trelliscope)
library(ggplot2, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

# create a data frame of metadata with a plot column
dat <- ggplot2::mpg |>
  tidyr::nest(data = !dplyr::one_of(c("manufacturer", "class"))) |>
  dplyr::mutate(
    mean_cty = purrr::map_dbl(data, function(x) mean(x$cty)),
    panel = map_plot(data, function(x) ggplot2::qplot(hwy, cty, data = x))
  )

dat
#> # A tibble: 32 × 5
#>    manufacturer class   data              mean_cty panel     
#>    <chr>        <chr>   <list>               <dbl> <trllscp_>
#>  1 audi         compact <tibble [15 × 9]>     17.9 <gg>      
#>  2 audi         midsize <tibble [3 × 9]>      16   <gg>      
#>  3 chevrolet    suv     <tibble [9 × 9]>      12.7 <gg>      
#>  4 chevrolet    2seater <tibble [5 × 9]>      15.4 <gg>      
#>  5 chevrolet    midsize <tibble [5 × 9]>      18.8 <gg>      
#>  6 dodge        minivan <tibble [11 × 9]>     15.8 <gg>      
#>  7 dodge        pickup  <tibble [19 × 9]>     12.1 <gg>      
#>  8 dodge        suv     <tibble [7 × 9]>      11.9 <gg>      
#>  9 ford         suv     <tibble [9 × 9]>      12.9 <gg>      
#> 10 ford         pickup  <tibble [7 × 9]>      13   <gg>      
#> # … with 22 more rows

disp <- dat |>
  trelliscope(name = "Highway mpg vs. City mpg") |>
  # provide information about metadata:
  add_meta_defs(
    meta_string("manufacturer", label = "Vehicle manufacturer name"),
    meta_factor("class", label = "Type of vehicle",
      levels = c("subcompact", "compact", "2seater", "midsize", "suv",
        "minivan", "pickup")),
    meta_number("mean_cty", label = "Mean city miles per gallon",
      tags = "metrics")
  ) |>
  # alternately can use add_meta_from_csv()
  # set the initial viewing state:
  set_labels(c("manufacturer", "class")) |>
  set_sort(c("class", "mean_cty"), dir = c("asc", "asc")) |>
  set_layout(nrow = 2, ncol = 3) |>
  set_filters(
    filter_string("manufacturer", values = c("audi", "volkswagen")),
    filter_range("mean_cty", min = 20)
  )
  # set_views()
  # set_inputs()
  # write_display()
```
