Display <- R6::R6Class(
  public = list(
    df = NULL,
    path = NULL,
    force_plot = NULL,
    panel_col = NULL,
    initialize = function(df, name, description, path, force_plot) {
      assertthat::assert_that(inherits(df, "data.frame"),
        msg = "Argument 'df' must be a data frame")
      self$df <- df
      check_scalar(name, "name")
      check_character(name, "name")
      check_scalar(description, "description")
      check_character(description, "description")
      check_scalar(path, "path")
      check_character(path, "path")
      check_scalar(force_plot, "force_plot")
      check_logical(force_plot, "force_plot")
      private$name <- name
      private$description <- description
      self$path <- path
      self$force_plot <- force_plot
      self$panel_col <- check_and_get_panel_col(df)
      private$state <- DisplayState$new()
    },
    set_meta = function(obj) {
      assertthat::assert_that(inherits(obj, "trelliscope_meta_def"),
        msg = "Meta variable definition must come from a meta_*() function")
      obj$check_with_data(self$df)
      name <- obj$get("varname")
      if (!is.null(private$metas[[name]]))
        message("Replacing existing meta variable definition for ", name)
      private$metas[[name]] <- obj
    },
    set_metas = function(objs) {
      for (obj in objs)
        self$set_meta(obj)
    },
    set_state = function(obj) {
      private$state <- obj
    },
    get = function(name) {
      private[[name]]
    }
  ),
  private = list(
    name = NULL,
    description = NULL,
    metas = list(),
    inputs = list(),
    state = NULL,
    views = list()
  )
)

#' Instantiate a trelliscope display object
#' @param df A data frame that contains the metadata of the display as well as
#' a column that indicate the panels to be displayed.
#' @param name Name of the trelliscope display.
#' @param description Description of the trelliscope display.
#' @param path Directory in which to place the trelliscope display when
#' it is written using [`write_display()`].
#' @param force_plot Should the panels be forced to be plotted, even if they
#' have already been plotted and have not changed since the previous plotting?
#' @export
trelliscope <- function(
  df, name, description = name, path = tempfile(), force_plot = FALSE
) {
  obj <- Display$new(df = df, name = name, description = description,
    path = path, force_plot = force_plot)
  class(obj) <- c("R6", "trelliscope_display")
  obj
}

check_and_get_panel_col <- function(df) {
  # look for a column with one of the following classes:
  # - img_panel (which includes img_panel_local)
  # - trelliscope_panels
  panel_col_idx <- which(unlist(lapply(df, function(a)
    inherits(a, c("img_panel", "trelliscope_panels")))))
  if (length(panel_col_idx) > 1) {
    message("Found multiple columns that indicate a panel, using the first ",
      " one found: '", names(panel_col_idx)[1], "'")
    panel_col_idx <- panel_col_idx[1]
  }
  assertthat::assert_that(length(panel_col_idx) == 1,
    msg = paste0("Couldn't find a column in the trelliscope input data frame ",
      "that references a plot or image."))
  names(panel_col_idx)
}
