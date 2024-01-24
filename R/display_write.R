#' Write the contents of a display
#' @param trdf A trelliscope data frame created with [`as_trelliscope_df()`]
#' or a data frame which will be cast as such.
#' @param force_write Should the panels be forced to be written even if they
#'   have already been written?
#' @param jsonp If true, app files are written as "jsonp" format, otherwise
#'   "json" format. The "jsonp" format makes it possible to browse a
#'   trelliscope app without the need for a web server.
#' @examples
#' library(ggplot2)
#'
#' panel_dat <- (
#'   ggplot(gap, aes(year, life_exp)) +
#'     geom_point() +
#'     facet_panels(vars(country, continent))
#' ) |>
#'   as_panels_df()
#'
#' disp <- panel_dat |>
#'   as_trelliscope_df(name = "life_expectancy")
#'
#' \dontrun{
#' disp <- write_trelliscope(disp)
#' view_trelliscope(disp)
#' }
#' @export
write_trelliscope <- function(
  trdf, force_write = NULL, jsonp = NULL
) {
  # in case new panel-like variables were added
  trdf <- find_panel_vars(trdf, warn = FALSE)

  trdf <- check_trelliscope_df(trdf)
  trobj <- attr(trdf, "trelliscope")$clone()

  if (!dir.exists(trobj$get_display_path()))
    dir.create(trobj$get_display_path(), recursive = TRUE)

  if (is.null(force_write))
    force_write <- trobj$force_plot
  if (is.null(jsonp))
    jsonp <- trobj$jsonp

  check_scalar(force_write, "force_write")
  check_logical(force_write, "force_write")
  check_scalar(jsonp, "jsonp")
  check_logical(jsonp, "jsonp")

  cfg <- check_app_config(trobj$path, jsonp, attr(trdf, "theme"))
  cfg_jsonp <- cfg$datatype == "jsonp"
  if (cfg_jsonp != jsonp) {
    jsonp <- cfg_jsonp
    msg("Using jsonp=", as.character(jsonp),
      " as it has already been specified in config")
  }

  # if (is.null(port))
  port <- httpuv::randomPort()

  trdf <- infer(trdf)

  trobj <- attr(trdf, "trelliscope")

  primary_panel <- trobj$get("primarypanel")

  trobj <- attr(trdf, "trelliscope")$clone()

  trdf_out <- trdf
  attr(trdf_out, "trelliscope") <- NULL

  panel_opts <- trobj$panel_options

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
        write_panels(trdf, nm, force = force_write)
      } else if (inherits(src, "LocalWebSocketPanelSource")) {
        if (is.null(src$get_port()))
          src$set_port(port)
      }
      trdf_out[[nm]] <- get_panel_rel_path(trdf[[nm]], nm,
        panel_opts[[nm]]$format)
    }
  }

  if (!is.null(primary_panel))
    trobj$set("primarypanel", primary_panel)
  if (is.null(trobj$get("thumbnailurl")))
    trobj$set("thumbnailurl", trdf_out[[primary_panel]][1])

  attr(trdf, "trelliscope") <- trobj

  write_trelliscope_info(trdf, jsonp, cfg$id)
  write_meta_data(trdf_out, trobj$df_cols_ignore, trobj$get_display_path(),
    jsonp, cfg$id)
  update_display_list(trobj$path, jsonp, cfg$id)

  if (!is.null(trobj$info_html_file))
    file.copy(trobj$info_html_file,
      file.path(trobj$get_display_path(), "info.html"),
        overwrite = TRUE)

  write_widget(trobj)

  invisible(trdf)
}

write_meta_data <- function(df, cols_ignore, disp_path, jsonp, id) {
  trobj <- attr(df, "trelliscope")
  df <- dplyr::select(df, !dplyr::all_of(cols_ignore))

  # txt <- get_jsonp_text(jsonp, paste0("__loadMetaData__", id))
  # cat(paste0(
  #   txt$st,
  #   as.character(to_json(df, factor = "integer", force = TRUE)),
  #   txt$nd
  # ), file = file.path(disp_path,
  #   paste0("metaData.", ifelse(jsonp, "jsonp", "json"))))
  # if (jsonp) {
  cat(paste0(
    "window.metaData = ",
    as.character(to_json(df, factor = "integer", force = TRUE))
  ), file = file.path(disp_path, "metaData.js"))
  # } else {
  #   cat(to_json(df, factor = "integer", force = TRUE),
  #     file = file.path(disp_path, "metaData.json"))
  # }
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
  tmp <- readLines(f, warn = FALSE) |> paste(collapse = "\n")
  rgxp <- paste0("^__[a-zA-Z]+__[a-zA-Z0-9_/\\.]+\\((.*)\\)")
  if (grepl("json$", f)) {
    res <- jsonlite::fromJSON(tmp)
  } else if (grepl("jsonp$", f)) {
    res <- jsonlite::fromJSON(gsub(rgxp, "\\1", tmp))
  }
  res
}

check_app_config <- function(app_path, jsonp, theme) {
  f <- list.files(app_path, pattern = "^config\\.json", full.names = TRUE)

  if (!is.null(theme) && inherits(theme, "trelliscope_theme")) {
    theme <- unclass(theme)
  } else {
    theme <- NULL
  }

  if (length(f) > 0) {
    cfg <- read_json_p(f[1])
    cfg$theme <- theme
  } else {
    cfg <- list(
      name = "Trelliscope App",
      datatype = ifelse(jsonp, "jsonp", "json"),
      id = substr(hash(Sys.time()), 1, 8)
    )
    cfg$theme <- theme
  }
  txt <- get_jsonp_text(jsonp, paste0("__loadAppConfig__", cfg$id))
  cat(paste0(txt$st, as.character(to_json(cfg, pretty = TRUE)), txt$nd),
    file = file.path(app_path,
      paste0("config", ifelse(jsonp, ".jsonp", ".json"))))
  cat(cfg$id, file = file.path(app_path, "id"))
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
    if (is.null(cur$order))
      cur$order <- 0
    cur[c("name", "description", "tags", "thumbnailurl", "order")]
  })
  odr <- order(unlist(lapply(lst, function(x) x$order)))

  txt <- get_jsonp_text(jsonp, paste0("__loadDisplayList__", id))
  cat(paste0(txt$st, as.character(to_json(lst[odr], pretty = TRUE)), txt$nd),
    file = file.path(app_path, "displays",
      paste0("displayList", ifelse(jsonp, ".jsonp", ".json"))))
}
