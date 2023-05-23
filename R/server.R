#' Add a plot column to a dataset
#' @param plot_fn A function that produces a panel from a given subset of `data`
#' @param data A data frame from which subsets will be extracted and plots will
#'   be made. Should be a superset of the summary dataset to which this plot
#'   column is being added.
#' @param by A list of variables found in both `data` and in the summary
#'   dataset to which this plot column is being added. This is used to specify
#'   which subset of `data` to apply for a given plot.
#' @param width Width in pixels of each panel.
#' @param height Height in pixels of each panel.
#' @param format The format of the panels the server will provide. Can be
#'   one of "png" , "svg", or "html".
#' @param force Should server force panels to be written? If `FALSE`, if the
#'   panel has already been generated, that is what will be made available.
#' @param prerender If "TRUE", the plots must be rendered prior to viewing the
#'   display. If "FALSE", a local R websockets server will be created and plots
#'   will be rendered on the fly when requested by the app. The latter is only
#'   available when using Trelliscope locally.
#' @export
plot_column <- function(
  plot_fn, data, by, width = 500, height = 500, format = "png",
  force = FALSE, prerender = TRUE
) {
  structure(
    "[not generated]",
      plot_column = list(
      d = data,
      plot_fn = plot_fn,
      width = width,
      height = height,
      format = "png",
      force = force,
      prerender = prerender
    ),
    class = "plot_column"
  )
}

#' @importFrom dplyr .data
#' @importFrom httpuv startServer randomPort
get_plot <- function(
  row, d, ds, plot_fn, key_cols, base_path, panel_path, rel_path, width, height,
  format, force
) {
  keydat <- ds[row, key_cols]
  key <- paste(unname(unlist(lapply(keydat, as.character))), collapse = "_")
  rel_path <- file.path(rel_path, paste0(key, ".", format))
  f <- file.path(panel_path, paste0(key, ".", format))
  if (file.exists(f) && !force) {
    message("File ", f, " exists, skipping plotting...")
    return(rel_path)
  }
  message("Plotting ", f, "...")
  nd <- d
  for (gv in key_cols) {
    nd <- dplyr::filter(nd, .data[[gv]] == ds[[gv]][[row]])
  }
  nd <- dplyr::collect(nd)
  p <- plot_fn(nd)
  write_panel(p, key, base_path, panel_path, width, height)
  rel_path
}

view_ws_trelliscope <- function(trdf, port = 8080) { # httpuv::randomPort()) {
  trs <- attr(trdf, "trelliscope")

  panel_path <- file.path(trs$get_display_path(), "panels")
  if (!dir.exists(panel_path))
    dir.create(panel_path, recursive = TRUE)

  ws_get_plot <- function(row) {
    get_plot(
      row = as.integer(row),
      d = trs$server$d,
      ds = trdf,
      plot_fn = trs$server$plot_fn,
      key_cols = trs$get("keycols"),
      base_path = trs$path,
      panel_path = panel_path,
      rel_path = trs$get_panel_rel_path(),
      width = trs$server$width,
      height = trs$server$height,
      format = trs$server$format,
      force = trs$server$force
    )
  }

  s <- httpuv::startServer("127.0.0.1", port,
    list(
      onWSOpen = function(ws) {
        # The ws object is a WebSocket object
        cat("Server connection opened.\n")
        ws$onMessage(function(binary, message) {
          ws$send(ws_get_plot(message))
        })
        ws$onClose(function() {
          cat("Server connection closed.\n")
        })
      },
      staticPaths = list("/" = trs$path)
    )
  )
  utils::browseURL(paste0("http://localhost:", port))
  s
}