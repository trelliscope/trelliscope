write_panels <- function(trdf, nm, force = FALSE) {
  x <- attr(trdf, "trelliscope")

  pnls <- trdf[[nm]]

  if (inherits(pnls, "plot_column")) {
    # html_head <- NULL
    # if (inherits(trdf[[panel_col]][[1]], "htmlwidget")) {
    #   dir.create(file.path(app_path, "libs"), showWarnings = FALSE)
    #   html_head <- write_htmlwidget_deps(
    #     trdf[[panel_col]][[1]], app_path, panel_path)
    #   format <- "html"
    # }

    panel_path <- file.path(x$get_display_path(), "panels",
      sanitize(nm))
    if (!dir.exists(panel_path))
      dir.create(panel_path, recursive = TRUE)

    atrs <- attr(pnls, "plot_column")

    pnl_rel_dir <- x$get_panel_rel_dir(nm)
    pths <- get_panel_rel_path(trdf, atrs$by, pnl_rel_dir, atrs$format)
    if (atrs$force || force) {
      idxs <- seq_along(pths)
    } else {
      idxs <- which(!file.exists(pths))
    }

    cli::cli_progress_bar(paste("Writing", nm), total = length(idxs))

    for (idx in idxs) {
      cli::cli_progress_update()

      trdf[[nm]][idx] <- get_plot(
        row = idx,
        d = atrs$d,
        ds = trdf,
        plot_fn = atrs$plot_fn,
        key_cols = atrs$by,
        base_path = x$path,
        panel_path = panel_path,
        rel_dir = pnl_rel_dir,
        width = atrs$width,
        height = atrs$height,
        format = atrs$format,
        # html_head = html_head,
        force = atrs$force || force
      )
    }

    cli::cli_progress_done()
  }

  panel_path <- file.path(x$get_display_path(), "panels")
  if (inherits(pnls, "nested_panels")) {
    ff <- list.files(panel_path)
    ff <- tools::file_path_sans_ext(ff)
    keys <- apply(trdf[x$get("keycols")], 1,
      function(x) sanitize(paste(x, collapse = "_")))
    extra <- setdiff(keys, ff)
    assert(length(extra) == 0,
      msg = paste0("Found ", length(extra), " panel keys that do not have ",
        " a corresponding panel file."))
  }

  trdf
}

get_panel_rel_path <- function(ds, key_cols, rel_dir, format) {
  keydat <- ds[, key_cols]
  for (ii in seq_along(keydat))
    keydat[[ii]] <- as.character(keydat[[ii]])
  keys <- apply(keydat, 1, function(x)
    sanitize(paste(x, collapse = "_")))
  file.path(rel_dir, paste0(keys, ".", format))
}

#' @importFrom dplyr .data
#' @importFrom httpuv startServer randomPort
get_plot <- function(
  row, d, ds, plot_fn, key_cols, base_path, panel_path, rel_dir,
  width, height, format,
  html_head = NULL, force = FALSE, verbose = FALSE
) {
  file <- get_panel_rel_path(ds[row, , drop = FALSE], key_cols,
    rel_dir, format)
  if (file.exists(file) && !force) {
    if (verbose)
      message("File ", file, " exists, skipping plotting...")
    return(file)
  }
  if (verbose)
    message("Plotting ", file, "...")
  nd <- d
  for (gv in key_cols) {
    nd <- dplyr::filter(nd, .data[[gv]] == ds[[gv]][[row]])
  }
  nd <- dplyr::collect(nd)
  p <- plot_fn(nd)
  f <- file.path(base_path, file)
  write_panel(p, f, base_path, panel_path, width, height)
  file
}









#' Write panels
#' @param trdf A trelliscope data frame created with [`as_trelliscope_df()`]
#' or a data frame which will be cast as such.
#' @param width Width in pixels of each panel.
#' @param height Height in pixels of each panel.
#' @param format The format of the image if it is not an htmlwidget. Can be
#'   either "png" or "svg".
#' @param force Should the panels be forced to be written? If `FALSE`, the
#'   content of the panel column along with the `width`, `height`, and
#'   `format` parameters will be used to determine if the panel content matches
#'   panels that have already been written, in which case writing the panels
#'    will be skipped.
#' @param ... Other arguments passed to the plotting functions (either
#'   svglite::svglite or grDevices::png).
#' @note The size of panels will vary when shown in the viewer, but here the
#'   specification of height and width help determine the plot aspect ratio
#'   as well as the initial resolution to render plot text, etc. with.
#' @importFrom cli cli_progress_bar cli_progress_update cli_progress_done
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' panel_dat <- (
#'   ggplot(gapminder, aes(year, lifeExp)) +
#'     geom_point() +
#'     facet_panels(~country + continent)
#'   ) |>
#'   nest_panels()
#'
#' disp <- panel_dat |>
#'   as_trelliscope_df(name = "life_expectancy", path = tempfile()) |>
#'   write_panels(width = 800, height = 500, format = "svg")
#' }
#' @export
write_panels_old <- function(
  trdf, width = 500, height = 500, format = "png", force = FALSE, ...
) {
  trdf <- check_trelliscope_df(trdf)
  check_scalar(width, "width")
  check_pos_numeric(width, "width")
  check_scalar(height, "height")
  check_pos_numeric(height, "height")

  trobj <- attr(trdf, "trelliscope")$clone()
  app_path <- trobj$path
  # TODO: look at this
  # df <- trdf
  # if (inherits(df, "facet_panels"))
  #   df <- nest_panels(df)

  # TODO: revamp all of this!

  # panel_col <- check_and_get_panel_col(trdf)

  # panel_keys <- get_panel_paths_from_keys(trdf, format)

  # panel_path <- file.path(trobj$get_display_path(), "panels")

  # if (!dir.exists(panel_path)) {
  #   res <- dir.create(panel_path, recursive = TRUE)
  #   assert(res == TRUE,
  #     "Could not create directory for panels: {panel_path}")
  # }

  # html_head <- NULL
  # if (inherits(trdf[[panel_col]][[1]], "htmlwidget")) {
  #   dir.create(file.path(app_path, "libs"), showWarnings = FALSE)
  #   html_head <- write_htmlwidget_deps(
  #     trdf[[panel_col]][[1]], app_path, panel_path)
  #   format <- "html"
  # }

  # cur_hash <- hash(c(height, width, format, trdf[[panel_col]]))

  # trdf[["__PANEL_KEY__"]] <- panel_keys
  # if (is.null(trobj$get("keysig")))
  #   trobj$set("keysig", hash(sort(panel_keys)))

  # trobj$set("panelformat", format)

  # if (!force && file.exists(file.path(panel_path, "hash"))) {
  #   prev_hash <- readLines(file.path(panel_path, "hash"), warn = FALSE)[1]
  #   # need to grab aspect ratio from previous
  #   ff <- list.files(trobj$get_display_path(),
  #     pattern = "displayInfo\\.json", full.names = TRUE)
  #   if (prev_hash == cur_hash && length(ff) > 0) {
  #     msg("Current panel content matches panels that have already been \\
  #       written. Skipping panel writing. To override this, use \\
  #       write_panels(..., force = TRUE).")
  #     trobj$panels_written <- TRUE
  #     trobj$set("panelaspect", read_json_p(ff)$panelaspect)
  #     attr(trdf, "trelliscope") <- trobj
  #     return(trdf)
  #   }
  # }

  # cli::cli_progress_bar("Writing panels", total = length(panel_keys))

  # for (ii in seq_along(panel_keys)) {
  #   cli::cli_progress_update()
  #   write_panel(
  #     x = trdf[[panel_col]][[ii]],
  #     key = panel_keys[ii],
  #     base_path = app_path,
  #     panel_path = panel_path,
  #     width = width,
  #     height = height,
  #     format = format,
  #     html_head = html_head,
  #     ...
  #   )
  # }
  # cli::cli_progress_done()

  # cat(cur_hash, file = file.path(panel_path, "hash"))

  # trobj$panels_written <- TRUE
  # trobj$set("panelaspect", width / height)

  # attr(trdf, "trelliscope") <- trobj
  # trdf
}

get_panel_paths_from_keys <- function(trdf, format) {
  trobj <- attr(trdf, "trelliscope")
  keycols <- trobj$get("keycols")
  apply(trdf[, keycols], 1,
    function(df) sanitize(paste(df, collapse = "_")))
  # TODO: make sure that when sanitized, keys are still unique
}
