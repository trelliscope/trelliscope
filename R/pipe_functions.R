#' Add a meta variable definition to a trelliscope display
#' @param disp A trelliscope display object created with `trelliscope()`.
#' @param obj A meta variable definition created with a meta_*() function
#' @export
add_meta_def <- function(disp, obj) {
  check_display_object(disp)
  disp2 <- disp$clone()
  disp2$set_meta(obj)
  disp2
}

#' Add multiple meta variable definitions to a trelliscope display
#' @param disp A trelliscope display object created with `trelliscope()`.
#' @param ... Any number of objects created with meta_*() functions
#' @export
add_meta_defs <- function(disp, ...) {
  check_display_object(disp)
  objs <- list(...)
  disp2 <- disp$clone()
  disp2$set_metas(objs)
  disp2
}

#' Add a layout state specification to a trelliscope display
#' @param disp A trelliscope display object created with `trelliscope()`.
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
#' @param disp A trelliscope display object created with `trelliscope()`.
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
#' @param disp A trelliscope display object created with `trelliscope()`.
#' @param varnames A vector of variable names to sort on.
#' @param dirs A vector of directions to sort on ("asc" or "desc").
#' @param add Should an existing sort specification be added to? If FALSE
#' (default), the entire sort specification will be overridden.
#' @export
set_sort <- function(disp, varnames, dirs, add = FALSE) {
  check_display_object(disp)
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

# set_filter <- function(disp, obj) {
# }

# set_filters <- function(disp, ...) {
# }

# set_view
# set_views

# add_input
