cast_var <- function(df, obj) {
  lbl <- attr(df[[obj$get("varname")]], "label")
  df <- obj$cast_variable(df)
  if (!is.null(lbl))
    attr(df[[obj$get("varname")]], "label") <- lbl
  df
}

#' Add a layout state specification to a trelliscope display
#' @param trdf A trelliscope data frame created with [`as_trelliscope_df()`]
#' or a data frame which will be cast as such.
#' @inheritParams state_layout
#' @export
set_default_layout <- function(
  trdf, ncol = 1, page = 1, sidebar = FALSE, visible_filters = NULL
) {
  trdf <- check_trelliscope_df(trdf)
  obj <- state_layout(ncol = ncol, page = page, sidebar = sidebar,
    visible_filters = visible_filters)
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
#' @param email An email address (optional).
#' @param vars A vector of meta variable names found in the display. These
#'   will be made available as columns in the csv download of user inputs.
#' @export
add_inputs <- function(trdf, ..., email = NULL, vars = NULL) {
  trdf <- check_trelliscope_df(trdf)
  trobj <- attr(trdf, "trelliscope")$clone()
  inputs <- list(...)
  for (inpt in inputs) {
    assert(inherits(inpt, "trelliscope_input_def"),
      msg = "Can only add input definitions to add_inputs()")
    trobj$set_input(inpt)
  }

  itfc <- trobj$get("inputs")$get("feedbackInterface")
  itfc$set("feedbackEmail", email)

  if (!is.null(vars)) {
    nms <- trobj$get_meta_names(trdf)
    assert(all(vars %in% nms), msg = "In `add_input_vars()`, 'vars' can only
      be valid meta variables that are found in the data.")
    itfc$set("includeMetaVars", vars)
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

#' Set the primary panel of a trelliscope display
#' @param trdf A trelliscope data frame created with [`as_trelliscope_df()`]
#' or a data frame which will be cast as such.
#' @param name The name of the panel to set as the primary panel that will be
#'   viewed when launching the app.
#' @export
set_primary_panel <- function(trdf, name) {
  trdf <- check_trelliscope_df(trdf)
  if (name %in% names(trdf) && inherits(trdf[[name]], panel_classes)) {
    trobj <- attr(trdf, "trelliscope")$clone()
    trobj$set("primarypanel", name)
    attr(trdf, "trelliscope") <- trobj
  } else {
    wrn("Panel '{name}' not found in data. Ignoring.")
  }
  trdf
}

#' Show "display info" when display first loads
#' @param trdf A trelliscope data frame created with [`as_trelliscope_df()`]
#' or a data frame which will be cast as such.
#' @param show Should display info be shown on load?
#' @export
set_show_info_on_load <- function(trdf, show = TRUE) {
  trdf <- check_trelliscope_df(trdf)
  trobj <- attr(trdf, "trelliscope")$clone()
  trobj$set("infoOnLoad", show)
  attr(trdf, "trelliscope") <- trobj
  trdf
}

#' Specify custom "display info" html
#' @param trdf A trelliscope data frame created with [`as_trelliscope_df()`]
#' or a data frame which will be cast as such.
#' @param file Path to an existing html file to use.
#' @export
set_info_html <- function(trdf, file) {
  trdf <- check_trelliscope_df(trdf)
  trobj <- attr(trdf, "trelliscope")$clone()
  trobj$set("hasCustomInfo", TRUE)
  assert(file.exists(file) && !dir.exists(file),
    msg = "File {file} does not exist.")
  assert(tools::file_ext(file) %in% c("htm", "html"),
    msg = "File {file} must be an HTML file.")
  trobj$info_html_file <- file
  attr(trdf, "trelliscope") <- trobj
  trdf
}

#' Set a color theme for a Trelliscope display
#' @param trdf A trelliscope data frame created with [`as_trelliscope_df()`]
#' @param primary The primary color which applies to the main controls in the
#'  app (buttons, navigation icons, checkboxes, etc.).
#' @param primary2 The second-level primary color which applies to hover states,
#'  etc. Typically a slightly lighter shade of the primary color.
#' @param primary3 The third-level primary color, mainly used for the fullscreen
#'  button. Typically a slightly darker shade of the primary color.
#' @param background The background color of the app.
#' @param background2 The second-level background color used in various
#'  components and headers. Typically a slightly lighter or darker shade of the
#'  background color.
#' @param background3 The third-level background color used for inactive filter
#'  bars, sub-header background, component outlines, etc. Typically a slightly
#'  lighter or darker shade of the background color.
#' @param bars Color used for highlighted bars in the filter graphs (histogram
#'  and bar chart).
#' @param text The general text color used in the app.
#' @param text2 The secondary text color used in the header, etc. Should be a
#'  contrasting color to the primary color.
#' @param button_text The text color used for buttons. Should be a
#'  contrasting color to the primary color.
#' @param text_disabled The text color used for disabled buttons, etc.
#' @param error The color used for error messages.
#' @param font_family The font family to use in the app. Default is "Poppins".
#'  Note that many aspects of the app are styled with this font, so changing it
#'  may result in less-attractive styling.
#' @param logo URL (relative or absolute) to a logo image to
#' include in the header.
#' @export
#' @examples
#' x <- mars_rover |>
#'   as_trelliscope_df(name = "mars rover") |>
#'   set_theme(
#'     primary = "#c80000",
#'     primary2 = "#f00000",
#'     primary3 = "#960000",
#'     background = "#222222",
#'     background2 = "#444444",
#'     background3 = "#333333",
#'     bars = "#c80000",
#'     text = "#ffffff",
#'     text2 = "#ffffff",
#'     text_disabled = "#bcbcbc",
#'     logo = rover_icon_b64
#'   )
set_theme <- function(
  trdf,
  primary = "#448aff",
  primary2 = "#4dabf5",
  primary3 = "#2e60b1",
  background = "#FEFEFE",
  background2 = "#EBEBEB",
  background3 = "#E0E0E0",
  bars = "#FFAE25",
  text = "#000",
  text2 = "#FFF",
  button_text = "#757575",
  text_disabled = "#BCBCBC",
  error = "#ff5252",
  font_family = "\"Poppins\", sans-serif",
  logo = NULL
) {
  trdf <- check_trelliscope_df(trdf)

  check_scalar(primary, "primary")
  check_scalar(primary2, "primary2")
  check_scalar(primary3, "primary3")
  check_scalar(background, "background")
  check_scalar(background2, "background2")
  check_scalar(background3, "background3")
  check_scalar(bars, "bars")
  check_scalar(text, "text")
  check_scalar(text2, "text2")
  check_scalar(text_disabled, "text_disabled")
  check_scalar(error, "error")
  check_scalar(font_family, "font_family")

  check_color(primary, "primary")
  check_color(primary2, "primary2")
  check_color(primary3, "primary3")
  check_color(background, "background")
  check_color(background2, "background2")
  check_color(background3, "background3")
  check_color(bars, "bars")
  check_color(text, "text")
  check_color(text2, "text2")
  check_color(text_disabled, "text_disabled")
  check_color(error, "error")
  check_character(font_family, "font_family")

  attr(trdf, "theme") <- structure(list(
    palette = list(
      primary = list(
        main = primary,
        light = primary2,
        dark = primary3,
        contrastText = button_text
      ),
      secondary = list(
        main = background,
        light = background2,
        dark = background3,
        contrastText = bars
      ),
      background = list(
        default = background
      ),
      action = list(
        active = button_text
      ),
      text = list(
        primary = text,
        secondary = text2,
        disabled = text_disabled
      ),
      error = list(
        main = error
      )
    ),
    typography = list(
      fontFamily = font_family,
      fontWeightLight = 200,
      fontWeightRegular = 300,
      fontWeightMedium = 400
    ),
    logo = logo
  ), class = c("list", "trelliscope_theme"))

  trdf
}
