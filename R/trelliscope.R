#' Instantiate a trelliscope display object
#' @param df A data frame that contains the metadata of the display as well as
#' a column that indicate the panels to be displayed.
#' @param name Name of the trelliscope display.
#' @param description Description of the trelliscope display.
#' @param tags Optional vector of tag names to identify the display in the
#'   case that there are many to search through.
#' @param key_cols Variable names in `df` that uniquely define a row of the
#'   data. If not supplied, an attempt will be made to infer them.
#' @param path Directory in which to place the trelliscope display when
#'   it is written using [`write_display()`].
#' @param force_plot Should the panels be forced to be plotted, even if they
#'   have already been plotted and have not changed since the previous plotting?
#' @export
#' @importFrom utils head
#' @importFrom dplyr group_cols
trelliscope <- function(
  df, name, description = name, key_cols = NULL, tags = NULL,
  path = tempfile(), force_plot = FALSE
) {
  if (inherits(df, "facet_trelliscope")) {
    # msg("
    #   An object from {.fn facet_trelliscope} was passed to {.fn trelliscope}.
    #   {.emph Building panels...}")
    msg("{.emph Note:} For more control over building panels, you can \\
      call {.fn build_panels} explicitly before passing to {.fn trelliscope}.",
      .frequency = "regularly", .frequency_id = "explicit_build_note")
    df <- build_panels(df)
  }

  panel_col <- check_and_get_panel_col(df)
  if (is.null(key_cols))
    key_cols <- get_key_cols(df)

  obj <- Display$new(df = df, name = name, description = description,
    key_cols = key_cols, path = path, force_plot = force_plot,
    panel_col = panel_col, tags = tags)
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
    msg("Found multiple columns that indicate a panel, using the first \\
      one found: '{names(panel_col_idx)[1]}")
    panel_col_idx <- panel_col_idx[1]
  }
  assert(length(panel_col_idx) == 1,
    msg = paste0("Couldn't find a column in the trelliscope input data frame ",
      "that references a plot or image."))
  names(panel_col_idx)
}

get_key_cols <- function(df) {
  if (!is.null(attr(df, "facet_cols"))) {
    key_cols <- attr(df, "facet_cols")
  } else if (!is.null(attr(df, "key_cols"))) {
    key_cols <- attr(df, "key_cols")
  } else if (inherits(df, "grouped_df")) {
    idx <- dplyr::group_cols(data = df)
    key_cols <- names(df)[idx]
    df <- dplyr::ungroup(df)
  } else {
    n <- nrow(df)
    nms <- names(df)
    char_cols <- which(unname(unlist(lapply(df, function(x)
      inherits(x, c("character", "factor"))))))
    key_cols <- character(0)
    for (ii in char_cols) {
      if (nrow(dplyr::distinct(df[utils::head(char_cols, ii)])) == n) {
        key_cols <- nms[utils::head(char_cols, ii)]
        break
      }
    }
    if (length(key_cols) == 0) {
      stop("Could not find columns of the data that uniquely define each ",
        "row.")
    }
  }

  if (is.null(attr(df, "facet_cols")))
    msg("Using the variable{?s} {.val {key_cols}} \\
      to uniquely identify each row of the data.")

  key_cols
}
