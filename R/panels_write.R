#' Write panels
#' @param disp A trelliscope display object created with [`trelliscope()`].
#' @param width Width in pixels of each panel.
#' @param height Height in pixels of each panel.
#' @param format The format of the image if it is not an htmlwidget. Can be
#'   either "png" or "svg".
#' @param force Should the panels be forced to be written? If `FALSE`, the
#'   content of the panel column along with the `width`, `height`, and
#'   `format` parameters will be used to determine if the panel content matches
#'   panels that have already been written, in which case writing the panels
#'    will be skipped.
#' @note The size of panels will vary when shown in the viewer, but here the
#'   specification of height and width help determine the plot aspect ratio
#'   as well as the initial resolution to render plot text, etc. with.
#' @importFrom cli cli_progress_bar cli_progress_update cli_progress_done
#' @importFrom rlang hash
#' @export
write_panels <- function(
  disp, width = 500, height = 500, format = "png", force = FALSE
) {
  check_display_object(disp)

  disp2 <- disp$clone()
  app_path <- disp2$path
  df <- disp2$df
  if (inherits(df, "facet_trelliscope"))
    df <- build_panels(df)

  panel_col <- check_and_get_panel_col(df)

  panel_keys <- get_panel_paths_from_keys(disp2, format)

  panel_path <- file.path(disp2$get_display_path(), "panels")

  if (!dir.exists(panel_path)) {
    res <- dir.create(panel_path, recursive = TRUE)
    assert(res == TRUE,
      "Could not create directory for panels: {panel_path}")
  }

  html_head <- NULL
  if (inherits(df[[panel_col]][[1]], "htmlwidget")) {
    dir.create(file.path(app_path, "libs"), showWarnings = FALSE)
    html_head <- write_htmlwidget_deps(
      df[[panel_col]][[1]], app_path, panel_path)
    format <- "html"
  }

  cur_hash <- rlang::hash(c(height, width, format, df[[panel_col]]))

  disp2$df[["__PANEL_KEY__"]] <- panel_keys
  if (is.null(disp2$get("key_sig")))
    disp2$set("key_sig", rlang::hash(sort(panel_keys)))

  disp2$set("panel_format", format)

  if (!force && file.exists(file.path(panel_path, "hash"))) {
    prev_hash <- readLines(file.path(panel_path, "hash"), warn = FALSE)[1]
    if (prev_hash == cur_hash) {
      msg("Current panel content matches panels that have already been \\
        written. Skipping panel writing. To override this, use \\
        write_panels(..., force = TRUE).")
      disp2$panels_written <- TRUE
      return(disp2)
    }
  }

  cli::cli_progress_bar("Writing panels", total = length(panel_keys))

  for (ii in seq_along(panel_keys)) {
    cli::cli_progress_update()
    write_panel(
      x = df[[panel_col]][[ii]],
      key = panel_keys[ii],
      base_path = app_path,
      panel_path = panel_path,
      width = width,
      height = height,
      format = format,
      html_head = html_head
    )
  }
  cli::cli_progress_done()

  cat(cur_hash, file = file.path(panel_path, "hash"))

  disp2$panels_written <- TRUE
  disp2
}

get_panel_paths_from_keys <- function(disp, format) {
  key_cols <- disp$get("key_cols")
  apply(disp$df[, key_cols], 1,
    function(df) sanitize(paste(df, collapse = "_")))
  # TODO: make sure that when sanitized, keys are still unique
}
