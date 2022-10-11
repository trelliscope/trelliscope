add_meta_class <- function(x) {
  class(x) <- c("R6", "trelliscope_meta_def")
  x
}

#' Specify a "string" metadata variable
#' @param varname Name of the variable.
#' @param description Description of the variable.
#' @param tags Vector of tag names that help classify this variable.
#' @family {metadata types}
#' @export
meta_string <- function(
  varname,
  description = varname,
  tags = "common"
) {
  StringMeta$new(
    varname = varname,
    description = description,
    tags = tags
  ) %>%
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
  description = varname,
  tags = "common",
  digits = NULL,
  locale = TRUE
) {
  NumberMeta$new(
    varname = varname,
    description = description,
    tags = tags,
    digits = digits,
    local = locale
  ) %>%
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
  description = varname,
  tags = "common",
  levels = NULL
) {
  FactorMeta$new(
    varname = varname,
    description = description,
    tags = tags,
    levels = levels
  ) %>%
  add_meta_class()
}

#' Specify a "date" metadata variable
#' @inheritParams meta_string
#' @family {metadata types}
#' @export
meta_date <- function(
  varname,
  description = varname,
  tags = "common"
) {
  DateMeta$new(
    varname = varname,
    description = description,
    tags = tags
  ) %>%
  add_meta_class()
}

#' Specify a "datetime" metadata variable
#' @inheritParams meta_string
#' @family {metadata types}
#' @export
meta_datetime <- function(
  varname,
  description = varname,
  tags = "common"
) {
  DatetimeMeta$new(
    varname = varname,
    description = description,
    tags = tags
  ) %>%
  add_meta_class()
}

#' Specify a "geo" metadata variable
#' @inheritParams meta_string
#' @param latvar Name of variable that contains the latitude.
#' @param longvar Name of variable that contains the longitude.
#' @family {metadata types}
#' @export
meta_geo <- function(
  varname,
  description = varname,
  tags = "common",
  latvar,
  longvar
) {
  GeoMeta$new(
    varname = varname,
    description = description,
    tags = tags,
    latvar = latvar,
    longvar = longvar
  ) %>%
  add_meta_class()
}

#' Specify a "graph" metadata variable
#' @inheritParams meta_string
#' @param idvarname Name of the variable in the data that the link items
#' specified in `varname` refer to.
#' @param direction Direction of the links specifed in `varname`. One of
#' "none", "to", or "from". Determines whether and how arrows are shown
#' in the network graph in the app.
#' @family {metadata types}
#' @export
meta_graph <- function(
  varname,
  description = varname,
  tags = "common",
  idvarname,
  direction = c("none", "to", "from")
) {
  direction <- match.arg(direction)
  GraphMeta$new(
    varname = varname,
    description = description,
    tags = tags,
    idvarname = idvarname,
    direction = direction
  ) %>%
  add_meta_class()
}

#' Specify a "href" metadata variable
#' @inheritParams meta_string
#' @family {metadata types}
#' @export
meta_href <- function(
  varname,
  description = varname,
  tags = "common"
) {
  HREFMeta$new(
    varname = varname,
    description = description,
    tags = tags
  ) %>%
  add_meta_class()
}
