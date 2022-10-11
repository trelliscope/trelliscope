add_state_class <- function(x) {
  class(x) <- c("R6", "trelliscope_state_def")
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
#' as labels
#' when viewing the display.
#' @export
state_labels <- function(varnames) {
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
