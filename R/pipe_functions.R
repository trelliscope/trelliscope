#' Add a meta variable definition to a trelliscope display
#' @param disp A trelliscope display object created with [`trelliscope()`].
#' @param obj A meta variable definition created with a meta_*() function.
#' @export
add_meta_def <- function(disp, obj) {
  check_display_object(disp)
  disp2 <- disp$clone()
  disp2$set_meta(obj)
  disp2
}

#' Add multiple meta variable definitions to a trelliscope display
#' @param disp A trelliscope display object created with [`trelliscope()`].
#' @param ... Any number of objects created with meta_*() functions.
#' @export
add_meta_defs <- function(disp, ...) {
  check_display_object(disp)
  objs <- list(...)
  disp2 <- disp$clone()
  disp2$set_metas(objs)
  disp2
}

#' Specify labels for meta variables
#' @param disp A trelliscope display object created with [`trelliscope()`].
#' @param ... A named set of labels, where each name must correspond to one
#' of the variables in the dataset
#' @details This function can be useful if you don't want to go to the trouble
#' of explicitly setting meta variable definitions but still want variable
#' descriptions.
add_meta_labels <- function(disp, ...) {
  check_display_object(disp)
  args <- list(...)
  assertthat::assert_that(length(names(args)) == length(args),
    msg = "Arguments must be named")
  names_diff <- setdiff(names(args), names(disp$df))
  assertthat::assert_that(length(names_diff) == 0,
    msg = paste0("The following variables are not in the data: ",
      paste(names_diff, collapse = ", ")))
  disp2 <- disp$clone()
  for (nm in names(args))
    disp2$meta_labels[[nm]] <- args[[nm]]
  disp2
}

#' Add a layout state specification to a trelliscope display
#' @param disp A trelliscope display object created with [`trelliscope()`].
#' @inheritParams state_layout
#' @export
set_layout <- function(disp, nrow = 1, ncol = 1, arrange = "rows", page = 1) {
  check_display_object(disp)
  obj <- state_layout(nrow = nrow, ncol = ncol, arrange = arrange, page = page)
  obj$check_with_data(disp$df)
  disp2 <- disp$clone()
  state <- disp2$get("state")
  state2 <- state$clone()
  state2$set(obj)
  disp2$set_state(state2)
  disp2
}

#' Add a labels state specification to a trelliscope display
#' @param disp A trelliscope display object created with [`trelliscope()`].
#' @inheritParams state_labels
#' @export
set_labels <- function(disp, varnames) {
  check_display_object(disp)
  obj <- state_labels(varnames = varnames)
  obj$check_with_data(disp$df)
  disp2 <- disp$clone()
  state <- disp2$get("state")
  state2 <- state$clone()
  state2$set(obj)
  disp2$set_state(state2)
  disp2
}

#' Add a labels state specification to a trelliscope display
#' @param disp A trelliscope display object created with [`trelliscope()`].
#' @param varnames A vector of variable names to sort on.
#' @param dirs A vector of directions to sort on ("asc" or "desc").
#' @param add Should an existing sort specification be added to? If FALSE
#' (default), the entire sort specification will be overridden.
#' @export
set_sort <- function(disp, varnames, dirs = "asc", add = FALSE) {
  check_display_object(disp)
  if (length(dirs) == 1)
    dirs <- rep(dirs, length(varnames))
  assertthat::assert_that(length(varnames) == length(dirs),
    msg = "In setting sort state, 'varnames' must have same length as 'dirs'")
  disp2 <- disp$clone()
  state <- disp2$get("state")
  state2 <- state$clone()
  for (ii in seq_along(varnames)) {
    obj <- state_sort(varname = varnames[ii], dir = dirs[ii])
    obj$check_with_data(disp$df)
    state2$set(obj, add = ii != 1 || add)
  }
  disp2$set_state(state2)
  disp2
}

#' Add a filter state specifications to a trelliscope display
#' @param disp A trelliscope display object created with [`trelliscope()`].
#' @param ... Filter state specifications (e.g. [`filter_string()`],
#' [`filter_range()`]).
#' @param add Should existing filter state specifications be added to?
#' Default is TRUE. If FALSE, the entire sort specification will be overridden.
#' @export
set_filters <- function(disp, ..., add = TRUE) {
  check_display_object(disp)
  objs <- list(...)
  disp2 <- disp$clone()
  state <- disp2$get("state")
  state2 <- state$clone()
  for (ii in seq_along(objs)) {
    assertthat::assert_that(inherits(objs[[ii]], "trelliscope_filter_def"),
      msg = "Can only add filter definitions to set_filter()")
    objs[[ii]]$check_with_data(disp$df)
    state2$set(objs[[ii]], add = ii != 1 || add)
  }
  disp2$set_state(state2)
  disp2
}

#' Add a view specification to a trelliscope display
#' @param name The name of the view.
#' @param ... Any number of state specifications that define the view. These
#' can be specified with any of [`state_layout()`], [`state_labels()`],
#' [`state_sort()`], [`filter_string()`], [`filter_range()`].
#' @param disp A trelliscope display object created with [`trelliscope()`].
#' @export
add_view <- function(disp, name, ...) {
  check_display_object(disp)
  view <- View$new(name, ...)
  disp2 <- disp$clone()
  view$check_with_data(disp$df)
  disp2$set_view(view)
  disp2
}

# add_input
