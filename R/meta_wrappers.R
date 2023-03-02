add_meta_class <- function(x) {
  class(x) <- c("R6", "trelliscope_meta_def")
  x
}

#' Specify a "string" metadata variable
#' @param varname Name of the variable.
#' @param label Description of the variable.
#' @param tags Vector of tag names that help classify this variable.
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
#' If `NULL`, all digits will be shown.
#' @param locale Should the variable be displayed using its local?
#' For example, 1234.56 in US would be displayed as 1,234.56.
#' @family {metadata types}
#' @export
meta_number <- function(
  varname,
  label = NULL,
  tags = NULL,
  digits = NULL,
  locale = TRUE
) {
  NumberMeta$new(
    varname = varname,
    label = label,
    tags = tags,
    digits = digits,
    locale = locale
  ) |>
  add_meta_class()
}

#' Specify a "currency" metadata variable
#' @inheritParams meta_string
#' @param code Currency code. See [currencies] for a list of possibilities.
#' @family {metadata types}
#' @export
meta_currency <- function(
  varname,
  label = NULL,
  tags = NULL,
  code = "USD"
) {
  CurrencyMeta$new(
    varname = varname,
    label = label,
    tags = tags,
    code = code
  ) |>
  add_meta_class()
}

#' Specify a "factor" metadata variable
#' @inheritParams meta_string
#' @param levels A vector of factor levels the correspond to the variable.
#' The order of levels will be respected when sorted, etc. If NULL,
#' levels will be inferred from the data based on the order they appear.
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
