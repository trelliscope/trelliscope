#' Write the contents of a display
#' @param trdf A trelliscope data frame created with [`as_trelliscope_df()`]
#' or a data frame which will be cast as such.
#' @param force_write Should the panels be forced to be written even if they
#'   have already been written?
#' @param jsonp If true, app files are written as "jsonp" format, otherwise
#'   "json" format. The "jsonp" format makes it possible to browse a
#'   trelliscope app without the need for a web server.
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
#'   write_panels() |>
#'   write_trelliscope() |>
#'   view_trelliscope()
#' }
#' @export
write_trelliscope <- function(
  trdf, force_write = FALSE, jsonp = TRUE
) {

  trdf <- check_trelliscope_df(trdf)
  trobj <- attr(trdf, "trelliscope")$clone()

  if (!dir.exists(trobj$get_display_path()))
    dir.create(trobj$get_display_path(), recursive = TRUE)

  cfg <- check_app_config(trobj$path, jsonp)
  cfg_jsonp <- cfg$datatype == "jsonp"
  if (cfg_jsonp != jsonp) {
    jsonp <- cfg_jsonp
    message("Using jsonp=", jsonp)
  }

  # if (is.null(port))
  port <- httpuv::randomPort()

  # TODO: rewrite this
  # is_server <- !is.null(trobj$server)
  # if (is_server) {
  #   srvobj <- LocalWebSocketPanelSource$new(port = httpuv::randomPort())
  #   trobj$set("panelsource", srvobj)
  #   srv <- trobj$server
  #   trobj$set("panelformat", srv$format)
  #   trobj$set("panelaspect", srv$width / srv$height)
  #   attr(trdf, "trelliscope") <- trobj
  # } else {
  #   writable <- !inherits(trdf[[trobj$panel_col]],
  #     c("img_panel", "iframe_panel"))
  #   if (writable && (!trobj$panels_written || force_write))
  #     trdf <- write_panels(trdf)
  # }

  trdf <- infer(trdf)

  trobj <- attr(trdf, "trelliscope")

  primary_panel <- NULL

  trobj <- attr(trdf, "trelliscope")$clone()

  # fill in missing panel info
  for (mt in trobj$get("metas")) {
    if (mt$get("type") == "panel") {
      if (is.null(primary_panel))
        primary_panel <- mt$get("varname")

      nm <- mt$get("varname")
      src <- mt$get("source")
      if (inherits(src, "FilePanelSource")) {
        # if any panels are not generated, generate them
        # check to see all panels exist
        trdf <- write_panels(trdf, nm, force = force_write)
      } else if (inherits(src, "LocalWebSocketPanelSource")) {
        if (is.null(src$get_port()))
          src$set_port(port)
      }
    }
  }

  if (is.null(trobj$get("primarypanel")))
    trobj$set("primarypanel", primary_panel)
  if (is.null(trobj$get("thumbnailurl")))
    trobj$set("thumbnailurl", trdf[[primary_panel]][1])

  attr(trdf, "trelliscope") <- trobj

  write_trelliscope_info(trdf, jsonp, cfg$id)
  write_meta_data(trdf, jsonp, cfg$id)
  update_display_list(trobj$path, jsonp, cfg$id)

  write_widget(trobj)

  invisible(trdf)
}

write_meta_data <- function(df, jsonp, id) {
  x <- attr(df, "trelliscope")

  df <- dplyr::select(df, !dplyr::all_of(x$df_cols_ignore))

  txt <- get_jsonp_text(jsonp, paste0("__loadMetaData__", id))
  cat(paste0(
    txt$st,
    as.character(to_json(df, factor = "integer", force = TRUE)),
    txt$nd
  ), file = file.path(x$get_display_path(),
    paste0("metaData.", ifelse(jsonp, "jsonp", "json"))))
}

write_trelliscope_info <- function(df, jsonp, id) {
  x <- attr(df, "trelliscope")

  display_info <- x$as_json(pretty = TRUE)

  txt <- get_jsonp_text(jsonp, paste0("__loadDisplayInfo__", id))
  cat(paste0(txt$st, as.character(display_info),
    txt$nd), file = file.path(x$get_display_path(),
      paste0("displayInfo.", ifelse(jsonp, "jsonp", "json"))))
}

get_jsonp_text <- function(jsonp, fn_name) {
  if (jsonp) {
    list(
      st = paste0(fn_name, "("),
      nd = ")"
    )
  } else {
    return(list(st = "", nd = ""))
  }
}

#' @importFrom jsonlite fromJSON
read_json_p <- function(f) {
  res <- NULL
  if (grepl("json$", f)) {
    res <- jsonlite::fromJSON(gsub(rgxp, "\\1", tmp))
  } else if (grepl("jsonp$", f)) {
    tmp <- readLines(f, warn = FALSE) |> paste(collapse = "\n")
    rgxp <- paste0("^__[a-zA-Z]+__[a-zA-Z0-9_/\\.]+\\((.*)\\)")
    res <- jsonlite::fromJSON(gsub(rgxp, "\\1", tmp))
  }
  res
}

check_app_config <- function(app_path, jsonp) {
  f <- list.files(app_path, pattern = "^config\\.json", full.names = TRUE)

  if (length(f) > 0) {
    cfg <- read_json_p(f[1])
  } else {
    cfg <- list(
      name = "Trelliscope App",
      datatype = ifelse(jsonp, "jsonp", "json"),
      id = substr(hash(Sys.time()), 1, 8)
    )
    txt <- get_jsonp_text(jsonp, paste0("__loadAppConfig__", cfg$id))
    cat(paste0(txt$st, as.character(to_json(cfg, pretty = TRUE)), txt$nd),
      file = file.path(app_path,
        paste0("config", ifelse(jsonp, ".jsonp", ".json"))))
    cat(cfg$id, file = file.path(app_path, "id"))
  }
  cfg
}

#' Update the list of all displays in an app directory
#' @param app_path The path where all of the displays are stored
#' @param jsonp If true, files are read and written as "jsonp" format,
#'   otherwise "json" format. The "jsonp" format makes it possible to browse a
#'   trelliscope app without the need for a web server.
#' @param id The id of the display. Can be found in `config.json[p]`.
#' @export
update_display_list <- function(app_path, jsonp = TRUE, id) {
  assert(dir.exists(file.path(app_path, "displays")),
    "The directory '{app_path}' does not contain any displays.")
  dispfile <- paste0("displayInfo.json", ifelse(jsonp, "p", ""))
  ff <- list.files(file.path(app_path, "displays"), full.names = TRUE)
  idx <- which(unlist(lapply(ff, function(f)
    dir.exists(f) && file.exists(file.path(f, dispfile)))))
  lst <- lapply(ff[idx], function(f) {
    cur <- read_json_p(file.path(f, dispfile))
    cur[c("name", "description", "tags", "keysig", "thumbnailurl")]
  })
  txt <- get_jsonp_text(jsonp, paste0("__loadDisplayList__", id))
  cat(paste0(txt$st, as.character(to_json(lst, pretty = TRUE)), txt$nd),
    file = file.path(app_path, "displays",
      paste0("displayList", ifelse(jsonp, ".jsonp", ".json"))))
}
