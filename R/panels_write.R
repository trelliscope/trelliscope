#' Write panels
#' @param disp A trelliscope display object created with [`trelliscope()`].
#' @param width Width in pixels of each panel.
#' @param height Height in pixels of each panel.
#' @param format The format of the image if it is not an htmlwidget. Can be
#'   either "png" or "svg".
#' @note The size of panels will vary when shown in the viewer, but here the
#'   specification of height and width help determine the plot aspect ratio
#'   as well as the initial resolution to render plot text, etc. with.
#' @export
panels_write <- function(disp, width = 500, height = 500, format = "png") {
  check_display_object(disp)

  disp2 <- disp$clone()
  name <- disp2$get("name")
  path <- disp2$path
  df <- disp2$df
  if (inherits(df, "facet_trelliscope"))
    df <- panels_build(df)

  panel_col <- check_and_get_panel_col(df)
  key_cols <- get_key_cols(df)
  panel_keys <- apply(df[, key_cols], 1,
    function(df) paste(df, collapse = "_"))

  panel_path <- file.path(path, "displays", name, "panels")

  if (!dir.exists(panel_path)) {
    res <- dir.create(panel_path, recursive = TRUE)
    if (!res)
      stop("Could not create directory for panels: ", panel_path)
  }

  html_head <- NULL
  if (inherits(df[[panel_col]][[1]], "htmlwidget")) {
    dir.create(file.path(path, "libs"), showWarnings = FALSE)
    html_head <- write_htmlwidget_deps(
      df[[panel_col]][[1]], path, panel_path)
  }

  pb <- progress::progress_bar$new(
    total = length(panel_keys), width = getOption("width") - 5,
    format = ":what [:bar] :percent :current/:total eta::eta")

  for (ii in seq_along(panel_keys)) {
    pb$tick(tokens = list(what = "writing panels      "))
    write_panel(
      x = df[[panel_col]][[ii]],
      key = panel_keys[ii],
      base_path = path,
      panel_path = panel_path,
      width = width,
      height = height,
      format = format,
      html_head = html_head
    )
  }
  disp2$panels_written <- TRUE
  disp2
}
