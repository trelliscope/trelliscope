add_state_class <- function(x, extra = NULL) {
  class(x) <- c("R6", "trelliscope_state_def", extra)
  x
}

#' Specify a "layout" state
#' @param nrow Number of rows of panels to show.
#' @param ncol Number of columns of panels to show.
#' @param arrange Arrange panels by "rows" or "cols".
#' @param page The page number to show.
#' @export
state_layout <- function(
  nrow = 1, ncol = 1, arrange = "rows", page = 1
) {
  LayoutState$new(
    nrow = nrow, ncol = ncol, arrange = arrange, page = 1
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
    stop("filter_range() must have one of min or max with numeric, ",
      "date, or datetime types.", call. = FALSE)
  }
  add_state_class(res, extra = "trelliscope_filter_def")
}
