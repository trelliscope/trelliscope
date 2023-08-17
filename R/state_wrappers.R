add_state_class <- function(x, extra = NULL) {
  class(x) <- c("R6", "trelliscope_state_def", extra)
  x
}

#' Specify a "layout" state
#' @param ncol Number of columns of panels to show.
#' @param page The page number to show.
#' @param sidebar Should the sidebar be shown?
#' @param viewtype The type of view to show ("grid" or "table").
#' @param visible_filters A vector of variable names to add as visible filters
#'   in the sidebar.
#' @export
state_layout <- function(
  ncol = 1, page = 1, sidebar = FALSE, viewtype = c("grid", "table"),
  visible_filters = NULL
) {
  viewtype <- match.arg(viewtype)
  LayoutState$new(
    ncol = ncol, page = 1, sidebar = sidebar, viewtype = viewtype,
    visible_filters = visible_filters
  ) |>
  add_state_class()
}

#' Specify a "labels" state
#' @param varnames A vector of variable names whose values should appear
#' as labels. If NULL, no labels will be displayed.
#' when viewing the display.
#' @export
state_labels <- function(varnames = NULL) {
  LabelState$new(varnames = varnames) |>
  add_state_class()
}

#' Specify a "sort" state
#' @param varname The name of the variable to sort on.
#' @param dir One of "asc" or "desc", describing the direction of the sort.
#' @export
state_sort <- function(varname, dir = "asc") {
  SortState$new(varname = varname, dir = dir) |>
  add_state_class()
}

#' Specify a string "filter" state (applies to string and factor meta
#' variables)
#' @param varname The name of the variable to filter on.
#' @param regexp A search string that can be a regular expression indicating
#' the values of the variable to filter on.
#' @param values A vector of specific values of the variable to filter on. If
#' `values` is specified, `regexp` will be ignored.
#' @export
filter_string <- function(varname, regexp = NULL, values = NULL) {
  CategoryFilterState$new(varname = varname,
    regexp = regexp, values = values) |>
  add_state_class(extra = "trelliscope_filter_def")
}

#' Specify a range "filter" state (applies to numeric, date, and datetime
#' meta variables)
#' @param varname The name of the variable to filter on.
#' @param min Lower bound of the range (if NULL, there is no lower bound).
#' @param max Upper bound of the range (if NULL, there is no upper bound).
#' @export
filter_range <- function(varname, min = NULL, max = NULL) {
  if (is.numeric(min) || is.numeric(max)) {
    res <- NumberRangeFilterState$new(varname, min = min, max = max)
  } else if (inherits(min, "Date") || inherits(max, "Date")) {
    res <- DateRangeFilterState$new(varname, min = min, max = max)
  } else if (inherits(min, "POSIXct") || inherits(max, "POSIXct")) {
    res <- DatetimeRangeFilterState$new(varname, min = min, max = max)
  } else {
    assert(FALSE,
      "filter_range() must have one of min or max with numeric, date, \
      or datetime types.")
  }
  add_state_class(res, extra = "trelliscope_filter_def")
}
