add_meta_class <- function(x) {
  class(x) <- c("R6", "trelliscope_meta_def")
  x
}

#' Specify a "string" metadata variable
#' @param varname Name of the variable.
#' @param label Description of the variable.
#' @param tags Vector of tag names that help classify this variable.
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' panel_dat <- (
#'   ggplot(gapminder, aes(year, lifeExp)) +
#'     geom_point() +
#'     facet_panels(~country + continent)
#'   ) |>
#'   nest_panels()
#'
#' meta_dat <- gapminder|>
#'   group_by(country, continent) |>
#'   summarise(
#'     mean_lifeexp = mean(lifeExp),
#'     min_lifeexp = min(lifeExp),
#'     max_lifeexp = max(lifeExp),
#'     mean_gdp = mean(gdpPercap),
#'     first_year = min(year),
#'     latitude = first(latitude),
#'     longitude = first(longitude),
#'     .groups = "drop"
#'   ) |>
#'   ungroup() |>
#'   mutate(
#'     first_date = as.Date(paste0(first_year, "-01-01")),
#'     first_datetime = as.POSIXct.Date(first_date),
#'     continent = as.factor(continent),
#'     wiki_link = paste0("https://en.wikipedia.org/wiki/", country)
#'   )
#'
#' joined_dat <- left_join(panel_dat, meta_dat,
#'   by = join_by(country, continent))
#'
#' trell <- joined_dat |>
#'   as_trelliscope_df() |>
#'   write_panels() |>
#'   add_meta_defs(
#'     meta_string(
#'       "country",
#'       label = "Name of the Country",
#'       tags = "More Info"
#'     )
#'   )
#' }
#' @family {metadata types}
#' @export
meta_string <- function(
  varname,
  label = NULL,
  tags = NULL
) {
  StringMeta$new(
    varname = varname,
    label = label,
    tags = tags
  ) |>
  add_meta_class()
}

#' Specify a "number" metadata variable
#' @inheritParams meta_string
#' @param digits How many digits to round to when displaying the number.
#' If not specified, a value will be inferred. If -1, all digits will be shown.
#' @param locale Should the variable be displayed using its locale?
#' For example, 1234.56 in US would be displayed as 1,234.56.
#' @param log Should the variable's distribution be shown on the log scale?
#' If not specified, an inference will be made based on its values.
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' panel_dat <- (
#'   ggplot(gapminder, aes(year, lifeExp)) +
#'     geom_point() +
#'     facet_panels(~country + continent)
#'   ) |>
#'   nest_panels()
#'
#' meta_dat <- gapminder|>
#'   group_by(country, continent) |>
#'   summarise(
#'     mean_lifeexp = mean(lifeExp),
#'     min_lifeexp = min(lifeExp),
#'     max_lifeexp = max(lifeExp),
#'     mean_gdp = mean(gdpPercap),
#'     first_year = min(year),
#'     latitude = first(latitude),
#'     longitude = first(longitude),
#'     .groups = "drop"
#'   ) |>
#'   ungroup() |>
#'   mutate(
#'     first_date = as.Date(paste0(first_year, "-01-01")),
#'     first_datetime = as.POSIXct.Date(first_date),
#'     continent = as.factor(continent),
#'     wiki_link = paste0("https://en.wikipedia.org/wiki/", country)
#'   )
#'
#' joined_dat <- left_join(panel_dat, meta_dat,
#'   by = join_by(country, continent))
#'
#' trell <- joined_dat |>
#'   as_trelliscope_df() |>
#'   write_panels() |>
#'   add_meta_defs(
#'     meta_number(
#'       "mean_lifeexp",
#'       label = "Average life expectancy across all measured years",
#'       tags = "More Info",
#'       digits = 2
#'     )
#'   )
#' }
#' @family {metadata types}
#' @export
meta_number <- function(
  varname,
  label = NULL,
  tags = NULL,
  digits = NULL,
  locale = TRUE,
  log = NULL
) {
  NumberMeta$new(
    varname = varname,
    label = label,
    tags = tags,
    digits = digits,
    locale = locale,
    log = log
  ) |>
  add_meta_class()
}

#' Specify a "currency" metadata variable
#' @inheritParams meta_string
#' @param code Currency code. See [currencies] for a list of possibilities.
#' @param digits How many digits to round to when displaying the number.
#' If `NULL`, all digits will be shown. Default is 2.
#' @param log Should the variable's distribution be shown on the log scale?
#' If not specified, an inference will be made based on its values.
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' panel_dat <- (
#'   ggplot(gapminder, aes(year, lifeExp)) +
#'     geom_point() +
#'     facet_panels(~country + continent)
#'   ) |>
#'   nest_panels()
#'
#' meta_dat <- gapminder|>
#'   group_by(country, continent) |>
#'   summarise(
#'     mean_lifeexp = mean(lifeExp),
#'     min_lifeexp = min(lifeExp),
#'     max_lifeexp = max(lifeExp),
#'     mean_gdp = mean(gdpPercap),
#'     first_year = min(year),
#'     latitude = first(latitude),
#'     longitude = first(longitude),
#'     .groups = "drop"
#'   ) |>
#'   ungroup() |>
#'   mutate(
#'     first_date = as.Date(paste0(first_year, "-01-01")),
#'     first_datetime = as.POSIXct.Date(first_date),
#'     continent = as.factor(continent),
#'     wiki_link = paste0("https://en.wikipedia.org/wiki/", country)
#'   )
#'
#' joined_dat <- left_join(panel_dat, meta_dat,
#'   by = join_by(country, continent))
#'
#' trell <- joined_dat |>
#'   as_trelliscope_df() |>
#'   write_panels() |>
#'   add_meta_defs(
#'     meta_currency(
#'       "mean_gdp",
#'       label = "Average GDP per capita",
#'       tags = "GDP"
#'     )
#'   )
#' }
#' @family {metadata types}
#' @export
meta_currency <- function(
  varname,
  label = NULL,
  tags = NULL,
  code = "USD",
  digits = 2,
  log = NULL
) {
  CurrencyMeta$new(
    varname = varname,
    label = label,
    tags = tags,
    code = code,
    log = log
  ) |>
  add_meta_class()
}

#' Specify a "factor" metadata variable
#' @inheritParams meta_string
#' @param levels A vector of factor levels the correspond to the variable.
#' The order of levels will be respected when sorted, etc. If NULL,
#' levels will be inferred from the data based on the order they appear.
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' panel_dat <- (
#'   ggplot(gapminder, aes(year, lifeExp)) +
#'     geom_point() +
#'     facet_panels(~country + continent)
#'   ) |>
#'   nest_panels()
#'
#' meta_dat <- gapminder|>
#'   group_by(country, continent) |>
#'   summarise(
#'     mean_lifeexp = mean(lifeExp),
#'     min_lifeexp = min(lifeExp),
#'     max_lifeexp = max(lifeExp),
#'     mean_gdp = mean(gdpPercap),
#'     first_year = min(year),
#'     latitude = first(latitude),
#'     longitude = first(longitude),
#'     .groups = "drop"
#'   ) |>
#'   ungroup() |>
#'   mutate(
#'     first_date = as.Date(paste0(first_year, "-01-01")),
#'     first_datetime = as.POSIXct.Date(first_date),
#'     continent = as.factor(continent),
#'     wiki_link = paste0("https://en.wikipedia.org/wiki/", country)
#'   )
#'
#' joined_dat <- left_join(panel_dat, meta_dat,
#'   by = join_by(country, continent))
#'
#' trell <- joined_dat |>
#'   as_trelliscope_df() |>
#'   write_panels() |>
#'   add_meta_defs(
#'     meta_factor(
#'       "continent",
#'       label = "Continent the country belongs in",
#'       tags = "Location"
#'     )
#'   )
#' }
#' @family {metadata types}
#' @export
meta_factor <- function(
  varname,
  label = NULL,
  tags = NULL,
  levels = NULL
) {
  FactorMeta$new(
    varname = varname,
    label = label,
    tags = tags,
    levels = levels
  ) |>
  add_meta_class()
}

#' Specify a "date" metadata variable
#' @inheritParams meta_string
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' panel_dat <- (
#'   ggplot(gapminder, aes(year, lifeExp)) +
#'     geom_point() +
#'     facet_panels(~country + continent)
#'   ) |>
#'   nest_panels()
#'
#' meta_dat <- gapminder|>
#'   group_by(country, continent) |>
#'   summarise(
#'     mean_lifeexp = mean(lifeExp),
#'     min_lifeexp = min(lifeExp),
#'     max_lifeexp = max(lifeExp),
#'     mean_gdp = mean(gdpPercap),
#'     first_year = min(year),
#'     latitude = first(latitude),
#'     longitude = first(longitude),
#'     .groups = "drop"
#'   ) |>
#'   ungroup() |>
#'   mutate(
#'     first_date = as.Date(paste0(first_year, "-01-01")),
#'     first_datetime = as.POSIXct.Date(first_date),
#'     continent = as.factor(continent),
#'     wiki_link = paste0("https://en.wikipedia.org/wiki/", country)
#'   )
#'
#' joined_dat <- left_join(panel_dat, meta_dat,
#'   by = join_by(country, continent))
#'
#' trell <- joined_dat |>
#'   as_trelliscope_df() |>
#'   write_panels() |>
#'   add_meta_defs(
#'     meta_date(
#'       "first_date",
#'       label = "January 1st of the first year data was recorded",
#'       tags = "Time"
#'     )
#'   )
#' }
#' @family {metadata types}
#' @export
meta_date <- function(
  varname,
  label = NULL,
  tags = NULL
) {
  DateMeta$new(
    varname = varname,
    label = label,
    tags = tags
  ) |>
  add_meta_class()
}

#' Specify a "datetime" metadata variable
#' @inheritParams meta_string
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' panel_dat <- (
#'   ggplot(gapminder, aes(year, lifeExp)) +
#'     geom_point() +
#'     facet_panels(~country + continent)
#'   ) |>
#'   nest_panels()
#'
#' meta_dat <- gapminder|>
#'   group_by(country, continent) |>
#'   summarise(
#'     mean_lifeexp = mean(lifeExp),
#'     min_lifeexp = min(lifeExp),
#'     max_lifeexp = max(lifeExp),
#'     mean_gdp = mean(gdpPercap),
#'     first_year = min(year),
#'     latitude = first(latitude),
#'     longitude = first(longitude),
#'     .groups = "drop"
#'   ) |>
#'   ungroup() |>
#'   mutate(
#'     first_date = as.Date(paste0(first_year, "-01-01")),
#'     first_datetime = as.POSIXct.Date(first_date),
#'     continent = as.factor(continent),
#'     wiki_link = paste0("https://en.wikipedia.org/wiki/", country)
#'   )
#'
#' joined_dat <- left_join(panel_dat, meta_dat,
#'   by = join_by(country, continent))
#'
#' trell <- joined_dat |>
#'   as_trelliscope_df() |>
#'   write_panels() |>
#'   add_meta_defs(
#'     meta_datetime(
#'       "first_datetime",
#'       label = "January 1st at 017:00 of the first year",
#'       tags = "Time"
#'     )
#'   )
#' }
#' @family {metadata types}
#' @export
meta_datetime <- function(
  varname,
  label = NULL,
  tags = NULL
) {
  DatetimeMeta$new(
    varname = varname,
    label = label,
    tags = tags
  ) |>
  add_meta_class()
}

#' Specify a "geo" metadata variable
#' @inheritParams meta_string
#' @param varname Name of the new variable that will act as geographical
#' coordinates (cannot exist in dataset).
#' @param latvar Name of variable that contains the latitude.
#' @param longvar Name of variable that contains the longitude.
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' panel_dat <- (
#'   ggplot(gapminder, aes(year, lifeExp)) +
#'     geom_point() +
#'     facet_panels(~country + continent)
#'   ) |>
#'   nest_panels()
#'
#' meta_dat <- gapminder|>
#'   group_by(country, continent) |>
#'   summarise(
#'     mean_lifeexp = mean(lifeExp),
#'     min_lifeexp = min(lifeExp),
#'     max_lifeexp = max(lifeExp),
#'     mean_gdp = mean(gdpPercap),
#'     first_year = min(year),
#'     latitude = first(latitude),
#'     longitude = first(longitude),
#'     .groups = "drop"
#'   ) |>
#'   ungroup() |>
#'   mutate(
#'     first_date = as.Date(paste0(first_year, "-01-01")),
#'     first_datetime = as.POSIXct.Date(first_date),
#'     continent = as.factor(continent),
#'     wiki_link = paste0("https://en.wikipedia.org/wiki/", country)
#'   )
#'
#' joined_dat <- left_join(panel_dat, meta_dat,
#'   by = join_by(country, continent))
#'
#' trell <- joined_dat |>
#'   as_trelliscope_df() |>
#'   write_panels() |>
#'   add_meta_defs(
#'     meta_geo(
#'       "coordinates",
#'       label = "Lat and Long of the country's capital", 
#'       tags = "Location",
#'       latvar = "latitude",
#'       longvar = "longitude"
#'     )
#'   )
#' }
#' @family {metadata types}
#' @export
meta_geo <- function(
  varname,
  label = NULL,
  tags = NULL,
  latvar,
  longvar
) {
  GeoMeta$new(
    varname = varname,
    label = label,
    tags = tags,
    latvar = latvar,
    longvar = longvar
  ) |>
  add_meta_class()
}

#' Specify a "graph" metadata variable
#' @inheritParams meta_string
#' @param varname Name of the new variable that will act as a graph
#' (cannot exist in dataset).
#' @param idvarname Name of the variable in the data that identifies the
#' node that each entity belongs to.
#' @param linkidvarname Name of the variable in the data that contains the
#' identifier of the node that each entity links to
#' @param labelvarname Name of the variable in the data that is used to
#' label the nodes in the graph.
#' @param direction Direction of the links specifed in `varname`. One of
#' "none", "to", or "from". Determines whether and how arrows are shown
#' in the network graph in the app.
#' @family {metadata types}
#' @export
meta_graph <- function(
  varname,
  label = NULL,
  tags = NULL,
  idvarname,
  linkidvarname,
  labelvarname = idvarname,
  direction = c("none", "to", "from")
) {
  direction <- match.arg(direction)
  GraphMeta$new(
    varname = varname,
    label = label,
    tags = tags,
    idvarname = idvarname,
    linkidvarname = linkidvarname,
    labelvarname = labelvarname,
    direction = direction
  ) |>
  add_meta_class()
}

#' Specify a "href" metadata variable
#' @inheritParams meta_string
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' panel_dat <- (
#'   ggplot(gapminder, aes(year, lifeExp)) +
#'     geom_point() +
#'     facet_panels(~country + continent)
#'   ) |>
#'   nest_panels()
#'
#' meta_dat <- gapminder|>
#'   group_by(country, continent) |>
#'   summarise(
#'     mean_lifeexp = mean(lifeExp),
#'     min_lifeexp = min(lifeExp),
#'     max_lifeexp = max(lifeExp),
#'     mean_gdp = mean(gdpPercap),
#'     first_year = min(year),
#'     latitude = first(latitude),
#'     longitude = first(longitude),
#'     .groups = "drop"
#'   ) |>
#'   ungroup() |>
#'   mutate(
#'     first_date = as.Date(paste0(first_year, "-01-01")),
#'     first_datetime = as.POSIXct.Date(first_date),
#'     continent = as.factor(continent),
#'     wiki_link = paste0("https://en.wikipedia.org/wiki/", country)
#'   )
#'
#' joined_dat <- left_join(panel_dat, meta_dat,
#'   by = join_by(country, continent))
#'
#' trell <- joined_dat |>
#'   as_trelliscope_df() |>
#'   write_panels() |>
#'   add_meta_defs(
#'     meta_href(
#'       "wiki_link",
#'       label = "Wikipedia article on the country",
#'       tags = "More Info"
#'     )
#'   )
#' }
#' @family {metadata types}
#' @export
meta_href <- function(
  varname,
  label = NULL,
  tags = NULL
) {
  HrefMeta$new(
    varname = varname,
    label = label,
    tags = tags
  ) |>
  add_meta_class()
}
