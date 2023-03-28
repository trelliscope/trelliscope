cast_var <- function(df, obj) {
  lbl <- attr(df[[obj$get("varname")]], "label")
  df <- obj$cast_variable(df)
  if (!is.null(lbl))
    attr(df[[obj$get("varname")]], "label") <- lbl
  df
}

#' Add a meta variable definition to a trelliscope display
#' @param trdf A trelliscope data frame created with [`as_trelliscope_df()`]
#' or a data frame which will be cast as such.
#' @param obj A meta variable definition created with a meta_*() function.
#' @export
add_meta_def <- function(trdf, obj) {
  trdf <- check_trelliscope_df(trdf)
  trobj <- attr(trdf, "trelliscope")$clone()
  trobj$set_meta(obj, trdf)
  trdf <- cast_var(trdf, obj)
  attr(trdf, "trelliscope") <- trobj
  trdf
}

#' Add multiple meta variable definitions to a trelliscope display
#' @param trdf A trelliscope data frame created with [`as_trelliscope_df()`]
#' or a data frame which will be cast as such.
#' @param ... Any number of objects created with meta_*() functions.
#' @export
add_meta_defs <- function(trdf, ...) {
  trdf <- check_trelliscope_df(trdf)
  objs <- list(...)
  trobj <- attr(trdf, "trelliscope")$clone()
  trobj$set_metas(objs, trdf)
  for (obj in objs)
    trdf <- cast_var(trdf, obj)
  attr(trdf, "trelliscope") <- trobj
  trdf
}

#' Specify labels for meta variables
#' @param trdf A trelliscope data frame created with [`as_trelliscope_df()`]
#' or a data frame which will be cast as such.
#' @param ... A named set of labels, where each name must correspond to one
#' of the variables in the dataset
#' @details This function can be useful if you don't want to go to the trouble
#' of explicitly setting meta variable definitions but still want variable
#' descriptions.
#' @export
add_meta_labels <- function(trdf, ...) {
  trdf <- check_trelliscope_df(trdf)
  args <- list(...)
  assert(length(names(args)) == length(args),
    msg = "Arguments must be named")
  names_diff <- setdiff(names(args), names(trdf))
  assert(length(names_diff) == 0,
    msg = paste0("The following variables are not in the data: ",
      paste(names_diff, collapse = ", ")))
  trobj <- attr(trdf, "trelliscope")$clone()
  for (nm in names(args))
    trobj$meta_labels[[nm]] <- args[[nm]]
  attr(trdf, "trelliscope") <- trobj
  trdf
}

#' Specify tags for meta variables
#' @param trdf A trelliscope data frame created with [`as_trelliscope_df()`]
#' or a data frame which will be cast as such.
#' @param ... A named set of vectors of tags, where each name must correspond to
#' one of the variables in the dataset
#' @details This function can be useful if you don't want to go to the trouble
#' of explicitly setting meta variable definitions but still want variable
#' tags.
#' @export
add_meta_tags <- function(trdf, ...) {
  trdf <- check_trelliscope_df(trdf)
  args <- list(...)
  assert(length(names(args)) == length(args),
    msg = "Arguments must be named")
  names_diff <- setdiff(names(args), names(trdf))
  assert(length(names_diff) == 0,
    msg = paste0("The following variables are not in the data: ",
      paste(names_diff, collapse = ", ")))
  trobj <- attr(trdf, "trelliscope")$clone()
  for (nm in names(args))
    trobj$meta_tags[[nm]] <- args[[nm]]
  attr(trdf, "trelliscope") <- trobj
  trdf
}

#' Add a layout state specification to a trelliscope display
#' @param trdf A trelliscope data frame created with [`as_trelliscope_df()`]
#' or a data frame which will be cast as such.
#' @inheritParams state_layout
#' @export
set_default_layout <- function(trdf, nrow = 1, ncol = 1, arrange = "rows", page = 1) {
  trdf <- check_trelliscope_df(trdf)
  obj <- state_layout(nrow = nrow, ncol = ncol, arrange = arrange, page = page)
  obj$check_with_data(trdf)
  trobj <- attr(trdf, "trelliscope")$clone()
  state <- trobj$get("state")
  state2 <- state$clone()
  state2$set(obj)
  trobj$set_state(state2)
  attr(trdf, "trelliscope") <- trobj
  trdf
}

#' Add a labels state specification to a trelliscope display
#' @param trdf A trelliscope data frame created with [`as_trelliscope_df()`]
#' or a data frame which will be cast as such.
#' @inheritParams state_labels
#' @export
set_default_labels <- function(trdf, varnames) {
  trdf <- check_trelliscope_df(trdf)
  obj <- state_labels(varnames = varnames)
  obj$check_with_data(trdf)
  trobj <- attr(trdf, "trelliscope")$clone()
  state <- trobj$get("state")
  state2 <- state$clone()
  state2$set(obj)
  trobj$set_state(state2)
  attr(trdf, "trelliscope") <- trobj
  trdf
}

#' Add a labels state specification to a trelliscope display
#' @param trdf A trelliscope data frame created with [`as_trelliscope_df()`]
#' or a data frame which will be cast as such.
#' @param varnames A vector of variable names to sort on.
#' @param dirs A vector of directions to sort on ("asc" or "desc").
#' @param add Should an existing sort specification be added to? If FALSE
#' (default), the entire sort specification will be overridden.
#' @export
set_default_sort <- function(trdf, varnames, dirs = "asc", add = FALSE) {
  trdf <- check_trelliscope_df(trdf)
  if (length(dirs) == 1)
    dirs <- rep(dirs, length(varnames))
  assert(length(varnames) == length(dirs),
    msg = "In setting sort state, 'varnames' must have same length as 'dirs'")
  trobj <- attr(trdf, "trelliscope")$clone()
  state <- trobj$get("state")
  state2 <- state$clone()
  for (ii in seq_along(varnames)) {
    obj <- state_sort(varname = varnames[ii], dir = dirs[ii])
    obj$check_with_data(trdf)
    state2$set(obj, add = ii != 1 || add)
  }
  trobj$set_state(state2)
  attr(trdf, "trelliscope") <- trobj
  trdf
}

#' Add a filter state specifications to a trelliscope display
#' @param trdf A trelliscope data frame created with [`as_trelliscope_df()`]
#' or a data frame which will be cast as such.
#' @param ... Filter state specifications (e.g. [`filter_string()`],
#' [`filter_range()`]).
#' @param add Should existing filter state specifications be added to?
#' Default is TRUE. If FALSE, the entire sort specification will be overridden.
#' @export
set_default_filters <- function(trdf, ..., add = TRUE) {
  trdf <- check_trelliscope_df(trdf)
  objs <- list(...)
  trobj <- attr(trdf, "trelliscope")$clone()
  state <- trobj$get("state")
  state2 <- state$clone()
  for (ii in seq_along(objs)) {
    assert(inherits(objs[[ii]], "trelliscope_filter_def"),
      msg = "Can only add filter definitions to set_filter()")
    objs[[ii]]$check_with_data(trdf)
    state2$set(objs[[ii]], add = ii != 1 || add)
  }
  trobj$set_state(state2)
  attr(trdf, "trelliscope") <- trobj
  trdf
}

#' Add a view specification to a trelliscope display
#' @param trdf A trelliscope data frame created with [`as_trelliscope_df()`]
#' or a data frame which will be cast as such.
#' @param name The name of the view.
#' @param ... Any number of state specifications that define the view. These
#' can be specified with any of [`state_layout()`], [`state_labels()`],
#' [`state_sort()`], [`filter_string()`], [`filter_range()`].
#' @export
add_view <- function(trdf, name, ...) {
  trdf <- check_trelliscope_df(trdf)
  view <- View$new(name, ...)
  trobj <- attr(trdf, "trelliscope")$clone()
  view$check_with_data(trdf)
  trobj$set_view(view)
  attr(trdf, "trelliscope") <- trobj
  trdf
}

#' Add inputs to a trelliscope display
#' @param trdf A trelliscope data frame created with [`as_trelliscope_df()`]
#' or a data frame which will be cast as such.
#' @param ... Any number of input specifications. These can be specified with
#' any of [`input_number()`], [`input_radio()`], [`input_checkbox()`],
#' [`input_select()`], [`input_multiselect()`], [`input_text()`]
#' @export
add_inputs <- function(trdf, ...) {
  trdf <- check_trelliscope_df(trdf)
  trobj <- attr(trdf, "trelliscope")$clone()
  for (inpt in list(...)) {
    assert(inherits(inpt, "trelliscope_input_def"),
      msg = "Can only add input definitions to add_inputs()")
    trobj$set_input(inpt)
  }
  attr(trdf, "trelliscope") <- trobj
  trdf
}

#' Specify an email address to which input feedback can be sent
#' @param email An email address.
#' @param trdf A trelliscope data frame created with [`as_trelliscope_df()`]
#' or a data frame which will be cast as such.
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#' 
#' panel_dat <- (ggplot(gapminder, aes(year, lifeExp)) +
#'   geom_point() +
#'   facet_panels()) |>
#'   nest_panels()
#'   
#' trell <- panel_dat |>
#' as_trelliscope_df() |>
#' write_panels() |>
#' add_inputs(
#'   input_radio(name = "Radio Input", 
#'                label = "A space to add custom ranking for sorting",
#'                options = c("yes", "no"))) |>
#' add_input_email("johndoe@email.com") |>
#' write_trelliscope() |>
#' view_trelliscope()
#' }
#' 
#' @export
add_input_email <- function(trdf, email) {
  trdf <- check_trelliscope_df(trdf)
  trobj <- attr(trdf, "trelliscope")$clone()
  if (length(trobj$get("inputs")) == 0) {
    wrn("There are no inputs for this display. Ignoring `add_input_email()`")
  } else {
    inputs2 <- trobj$get("inputs")$clone()
    itfc <- inputs2$get("feedbackInterface")$clone()
    itfc$set("feedbackEmail", email)
    inputs2$set("feedbackInterface", itfc)
    trobj$set("inputs", inputs2)
  }
  attr(trdf, "trelliscope") <- trobj
  trdf
}

#' Specify meta variables whose values should be provided in input feedback
#' @param trdf A trelliscope data frame created with [`as_trelliscope_df()`]
#' or a data frame which will be cast as such.
#' @param vars A vector of meta variable names found in the display.
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#' 
#' panel_dat <- (ggplot(gapminder, aes(year, lifeExp)) +
#'   geom_point() +
#'   facet_panels()) |>
#'   nest_panels()
#'   
#' trell <- panel_dat |>
#' as_trelliscope_df() |>
#' write_panels() |>
#' add_inputs(
#'   input_radio(name = "Radio Input", 
#'                label = "A space to add custom ranking for sorting",
#'                options = c("yes", "no"))) |>
#' add_input_vars(c("continent", "country")) |>
#' add_input_email("johndoe@email.com") |>
#' write_trelliscope() |>
#' view_trelliscope()
#' }
#' 
#' 
#' @export
add_input_vars <- function(trdf, vars) {
  trdf <- check_trelliscope_df(trdf)
  trobj <- attr(trdf, "trelliscope")$clone()
  if (length(trobj$get("inputs")) == 0) {
    wrn("There are no inputs for this display. Ignoring `add_input_vars()`")
  } else {
    nms <- trobj$get_meta_names(trdf)
    assert(all(vars %in% nms), msg = "In `add_input_vars()`, 'vars' can only
      be valid meta variables that are found in the data.")
    inputs2 <- trobj$get("inputs")$clone()
    itfc <- inputs2$get("feedbackInterface")$clone()
    itfc$set("includeMetaVars", vars)
    inputs2$set("feedbackInterface", itfc)
    trobj$set("inputs", inputs2)
  }
  attr(trdf, "trelliscope") <- trobj
  trdf
}

#' Use fidelius to password protect a trelliscope display
#' @param trdf A trelliscope data frame created with [`as_trelliscope_df()`]
#' or a data frame which will be cast as such.
#' @param ... Arguments passed to the charm() function in the fidelius package.
#' @export
add_charm <- function(trdf, ...) {
  trdf <- check_trelliscope_df(trdf)
  trobj <- attr(trdf, "trelliscope")$clone()
  trobj$fidelius_pars <- list(...)
  attr(trdf, "trelliscope") <- trobj
  trdf
}

#' Convert any trelliscope R6 object to JSON
#' @param trdf A trelliscope data frame created with [`as_trelliscope_df()`]
#' or a data frame which will be cast as such.
#' @param pretty Adds indentation whitespace to JSON output. Can be TRUE/FALSE
#' or a number specifying the number of spaces to indent.
#' @examples 
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#' 
#' panel_dat <- (ggplot(gapminder, aes(year, lifeExp)) +
#'                 geom_point() +
#'                 facet_panels(~country + continent)) |>
#'   nest_panels()
#' 
#' trell <- panel_dat |>
#'   as_trelliscope_df(name = "life expectancy", path = "gapminder") |>
#'   set_default_layout(nrow = 2, ncol = 4) |>
#'   write_panels() |>
#'   write_trelliscope()
#'   
#' as_json(trell)
#'   
#' }
#' 
#' @export
as_json <- function(trdf, pretty = TRUE) {
  obj <- attr(trdf, "trelliscope")$clone()
  assert(inherits(obj, "R6"),
    msg = "as_json() only applies to R6 objects")
  assert(!is.null(obj$as_json),
    msg = "Object provided to as_json() must have its own as_json() method")
  if (inherits(obj, "trelliscope_object")) {
    trdf <- infer(trdf)
    obj <- attr(trdf, "trelliscope")
  }
  obj$as_json(pretty = pretty)
}
