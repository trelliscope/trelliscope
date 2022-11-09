#' Write panels
#' @param disp A trelliscope display object created with [`trelliscope()`].
#' @param width Width in pixels of each panel.
#' @param height Height in pixels of each panel.
#' @param format The format of the image if it is not an htmlwidget. Can be
#'   either "png" or "svg".
#' @note The size of panels will vary when shown in the viewer, but here the
#'   specification of height and width help determine the plot aspect ratio
#'   as well as the initial resolution to render plot text, etc. with.
#' @importFrom cli cli_progress_bar cli_progress_update cli_progress_done
#' @export
write_panels <- function(disp, width = 500, height = 500, format = "png") {
  check_display_object(disp)

  disp2 <- disp$clone()
  app_path <- disp2$path
  df <- disp2$df
  if (inherits(df, "facet_trelliscope"))
    df <- build_panels(df)

  panel_col <- check_and_get_panel_col(df)
  key_cols <- disp2$get("key_cols")
  panel_keys <- apply(df[, key_cols], 1,
    function(df) paste(df, collapse = "_"))

  panel_path <- file.path(disp2$get_display_path(), "panels")

  if (!dir.exists(panel_path)) {
    res <- dir.create(panel_path, recursive = TRUE)
    if (!res)
      stop("Could not create directory for panels: ", panel_path)
  }

  html_head <- NULL
  if (inherits(df[[panel_col]][[1]], "htmlwidget")) {
    dir.create(file.path(app_path, "libs"), showWarnings = FALSE)
    html_head <- write_htmlwidget_deps(
      df[[panel_col]][[1]], app_path, panel_path)
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

  disp2$panels_written <- TRUE
  disp2
}
