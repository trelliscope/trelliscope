
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

A simple visualization of city vs. highway mpg by car manufacturer and
class:

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
```

We can specify a trelliscope display from this as easily as:

``` r
disp <- dat |>
  trelliscope(name = "Highway mpg vs. City mpg")
#> Using the variable(s): 'manufacturer, class' to uniquely identify each row of the data.
```

But we can provide fine control over the app with the use of many
additional utility functions:

``` r
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
  set_labels(c("manufacturer", "class", "mean_cty")) |>
  set_sort(c("class", "mean_cty"), dir = c("asc", "asc")) |>
  set_layout(nrow = 2, ncol = 3) |>
  set_filters(
    filter_string("manufacturer", values = c("audi", "volkswagen")),
    filter_range("mean_cty", min = 20)
  ) |>
  add_view(
    name = "test view",
    state_layout(nrow = 3, ncol = 5),
    state_labels(c("class", "manufacturer")),
    state_sort("manufacturer"),
    state_sort("mean_cty", "desc"),
    filter_string("manufacturer",
      values = c("toyota", "honda", "nissan", "subaru")),
    filter_range("mean_cty", max = 20)
  ) |>
  add_view(
    name = "test view 2",
    state_sort("class"),
    filter_range("mean_cty", min = 15, max = 25)
  ) |>
  add_inputs(
    input_radio(name = "good_radio",
      label = "Is it good?", options = c("no", "yes")),
    input_checkbox(name = "good_checkbox",
      label = "Is it good?", options = c("no", "yes")),
    input_select(name = "good_select",
      label = "Is it good?", options = c("no", "yes")),
    input_multiselect(name = "good_multiselect",
      label = "Is it good?", options = c("no", "yes")),
    input_text(name = "opinion", label = "What do you think?",
      width = 100, height = 6),
    input_number(name = "rank", label = "Rank this panel")
  )
#> Using the variable(s): 'manufacturer, class' to uniquely identify each row of the data.
  # write_display()
```

To see what the JSON representation of this looks like:

``` r
disp |> as_json()
#> Note: Cannot find a data type for variable: data. This variable will not be available in the display.
#> Note: Cannot find a data type for variable: panel. This variable will not be available in the display.
#> The following variables had their meta definition inferred:
#> No layout definition supplied for view 'test view 2'. Using default.
#> No labels definition supplied for view 'test view 2'. Using default.
#> {
#>   "name": "Highway mpg vs. City mpg",
#>   "description": "Highway mpg vs. City mpg",
#>   "id_vars": ["manufacturer", "class"],
#>   "metas": [
#>     {
#>       "sortable": true,
#>       "filterable": true,
#>       "tags": [],
#>       "label": "Vehicle manufacturer name",
#>       "type": "string",
#>       "varname": "manufacturer"
#>     },
#>     {
#>       "levels": ["subcompact", "compact", "2seater", "midsize", "suv", "minivan", "pickup"],
#>       "sortable": true,
#>       "filterable": true,
#>       "tags": [],
#>       "label": "Type of vehicle",
#>       "type": "factor",
#>       "varname": "class"
#>     },
#>     {
#>       "locale": true,
#>       "digits": null,
#>       "sortable": true,
#>       "filterable": true,
#>       "tags": "metrics",
#>       "label": "Mean city miles per gallon",
#>       "type": "number",
#>       "varname": "mean_cty"
#>     }
#>   ],
#>   "state": {
#>     "layout": {
#>       "page": 1,
#>       "arrange": "rows",
#>       "ncol": 3,
#>       "nrow": 2,
#>       "type": "layout"
#>     },
#>     "labels": {
#>       "varnames": ["manufacturer", "class", "mean_cty"],
#>       "type": "labels"
#>     },
#>     "sort": [
#>       {
#>         "dir": "asc",
#>         "varname": "class",
#>         "type": "sort"
#>       },
#>       {
#>         "dir": "asc",
#>         "varname": "mean_cty",
#>         "type": "sort"
#>       }
#>     ],
#>     "filter": [
#>       {
#>         "values": ["audi", "volkswagen"],
#>         "regexp": null,
#>         "filtertype": "category",
#>         "varname": "manufacturer",
#>         "type": "filter"
#>       },
#>       {
#>         "max": null,
#>         "min": 20,
#>         "filtertype": "numberrange",
#>         "varname": "mean_cty",
#>         "type": "filter"
#>       }
#>     ]
#>   },
#>   "views": [
#>     {
#>       "name": "test view",
#>       "state": {
#>         "layout": {
#>           "page": 1,
#>           "arrange": "rows",
#>           "ncol": 5,
#>           "nrow": 3,
#>           "type": "layout"
#>         },
#>         "labels": {
#>           "varnames": ["class", "manufacturer"],
#>           "type": "labels"
#>         },
#>         "sort": [
#>           {
#>             "dir": "asc",
#>             "varname": "manufacturer",
#>             "type": "sort"
#>           },
#>           {
#>             "dir": "desc",
#>             "varname": "mean_cty",
#>             "type": "sort"
#>           }
#>         ],
#>         "filter": [
#>           {
#>             "values": ["toyota", "honda", "nissan", "subaru"],
#>             "regexp": null,
#>             "filtertype": "category",
#>             "varname": "manufacturer",
#>             "type": "filter"
#>           },
#>           {
#>             "max": 20,
#>             "min": null,
#>             "filtertype": "numberrange",
#>             "varname": "mean_cty",
#>             "type": "filter"
#>           }
#>         ]
#>       }
#>     },
#>     {
#>       "name": "test view 2",
#>       "state": {
#>         "layout": {
#>           "page": 1,
#>           "arrange": "rows",
#>           "ncol": 3,
#>           "nrow": 2,
#>           "type": "layout"
#>         },
#>         "labels": {
#>           "varnames": ["manufacturer", "class"],
#>           "type": "labels"
#>         },
#>         "sort": [
#>           {
#>             "dir": "asc",
#>             "varname": "class",
#>             "type": "sort"
#>           }
#>         ],
#>         "filter": [
#>           {
#>             "max": 25,
#>             "min": 15,
#>             "filtertype": "numberrange",
#>             "varname": "mean_cty",
#>             "type": "filter"
#>           }
#>         ]
#>       }
#>     }
#>   ],
#>   "inputs": [
#>     {
#>       "options": ["no", "yes"],
#>       "type": "radio",
#>       "active": true,
#>       "label": "Is it good?",
#>       "name": "good_radio"
#>     },
#>     {
#>       "options": ["no", "yes"],
#>       "type": "checkbox",
#>       "active": true,
#>       "label": "Is it good?",
#>       "name": "good_checkbox"
#>     },
#>     {
#>       "options": ["no", "yes"],
#>       "type": "select",
#>       "active": true,
#>       "label": "Is it good?",
#>       "name": "good_select"
#>     },
#>     {
#>       "options": ["no", "yes"],
#>       "type": "multiselect",
#>       "active": true,
#>       "label": "Is it good?",
#>       "name": "good_multiselect"
#>     },
#>     {
#>       "height": 6,
#>       "width": 100,
#>       "type": "text",
#>       "active": true,
#>       "label": "What do you think?",
#>       "name": "opinion"
#>     },
#>     {
#>       "type": "number",
#>       "active": true,
#>       "label": "Rank this panel",
#>       "name": "rank"
#>     }
#>   ]
#> }
```
