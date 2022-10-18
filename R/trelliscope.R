#' Instantiate a trelliscope display object
#' @param df A data frame that contains the metadata of the display as well as
#' a column that indicate the panels to be displayed.
#' @param name Name of the trelliscope display.
#' @param description Description of the trelliscope display.
#' @param id_vars Variable names in `df` that uniquely define a row of the
#' data. If not supplied, an attempt will be made to infer them.
#' @param path Directory in which to place the trelliscope display when
#' it is written using [`write_display()`].
#' @param force_plot Should the panels be forced to be plotted, even if they
#' have already been plotted and have not changed since the previous plotting?
#' @export
#' @importFrom utils head
trelliscope <- function(
  df, name, description = name, id_vars = NULL,
  path = tempfile(), force_plot = FALSE
) {
  panel_col <- check_and_get_panel_col(df)
  if (is.null(id_vars)) {
    n <- nrow(df)
    nms <- names(df)
    char_cols <- which(unname(unlist(lapply(df, function(x)
      inherits(x, c("character", "factor"))))))
    id_vars <- character(0)
    for (ii in char_cols) {
      if (nrow(dplyr::distinct(df[utils::head(char_cols, ii)])) == n) {
        id_vars <- nms[utils::head(char_cols, ii)]
        break
      }
    }
    if (length(id_vars) == 0) {
      message("Could not find columns of the data that uniquely define each ",
        "row. Creating a new variable '__id__' as an identifier")
      df[["__id__"]] <- seq_len(n)
      id_var <- "__id__"
    } else {
      message("Using the variable(s): '", paste0(id_vars, collapse = ", "),
        "' to uniquely identify each row of the data.")
    }
  }
  obj <- Display$new(df = df, name = name, description = description,
    id_vars = id_vars, path = path, force_plot = force_plot,
    panel_col = panel_col)
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
