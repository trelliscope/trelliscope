#' Write the contents of a display
#' @param disp A trelliscope display object created with [`trelliscope()`].
#' @param force_write Should the panels be forced to be written even if they
#'   have already been written?
#' @param jsonp If true, app files are written as "jsonp" format, otherwise
#'   "json" format. The "jsonp" format makes it possible to browse a
#'   trelliscope app without the need for a web server.
#' @export
write_display <- function(disp, force_write = FALSE, jsonp = TRUE) {
  check_display_object(disp)

  if (!dir.exists(disp$get_display_path()))
    dir.create(disp$get_display_path(), recursive = TRUE)

  cfg <- check_app_config(disp$path, jsonp)
  cfg_jsonp <- cfg$data_type == "jsonp"
  if (cfg_jsonp != jsonp) {
    jsonp <- cfg_jsonp
    message("Using jsonp=", jsonp)
  }

  x <- disp$clone()

  writable <- !inherits(x$df[[x$panel_col]], c("img_panel", "iframe_panel"))
  if ((!x$panels_written || force_write) && writable)
    x <- write_panels(x)

  x <- infer(x)
  check_panels(x)

  write_display_info(x, jsonp, cfg$id)
  write_meta_data(x, jsonp, cfg$id)
  update_display_list(x$path, jsonp, cfg$id)

  invisible(x)
}

write_meta_data <- function(x, jsonp, id) {
  df <- dplyr::select(x$df, !dplyr::all_of(x$df_cols_ignore))

  txt <- get_jsonp_text(jsonp, paste0("__loadMetaData__", id))
  cat(paste0(txt$st, as.character(to_json(df)), txt$nd),
    file = file.path(x$get_display_path(),
      paste0("metaData.", ifelse(jsonp, "jsonp", "json"))))
}

write_display_info <- function(x, jsonp, id) {
  display_info <- x$as_json(pretty = TRUE)

  txt <- get_jsonp_text(jsonp, paste0("__loadDisplayInfo__", id))
  cat(paste0(txt$st, as.character(display_info),
    txt$nd), file = file.path(x$get_display_path(),
      paste0("displayInfo.", ifelse(jsonp, "jsonp", "json"))))
}

check_panels <- function(x) {
  pnls <- x$df[[x$panel_col]]
  panel_path <- file.path(x$get_display_path(), "panels")
  if (inherits(pnls, "trelliscope_panels")) {
    ff <- list.files(panel_path)
    ff <- tools::file_path_sans_ext(ff)
    keys <- apply(x$df[x$get("key_cols")], 1,
      function(x) sanitize(paste(x, collapse = "_")))
    extra <- setdiff(keys, ff)
    assert(length(extra) == 0,
      msg = paste0("Found ", length(extra), " panel keys that do not have ",
        " a corresponding panel file."))
  }
  TRUE
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

#' @importFrom rlang hash
check_app_config <- function(app_path, jsonp) {
  f <- list.files(app_path, pattern = "^config\\.json", full.names = TRUE)

  if (length(f) > 0) {
    cfg <- read_json_p(f[1])
  } else {
    cfg <- list(
      name = "Trelliscope App",
      data_type = ifelse(jsonp, "jsonp", "json"),
      id = substr(rlang::hash(Sys.time()), 1, 8)
    )
    txt <- get_jsonp_text(jsonp, paste0("__loadAppConfig__", cfg$id))
    cat(paste0(txt$st, as.character(to_json(cfg, pretty = TRUE)), txt$nd),
      file = file.path(app_path,
        paste0("config", ifelse(jsonp, ".jsonp", ".json"))))
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
  if (!dir.exists(file.path(app_path, "displays")))
    stop("The directory '", app_path, "' does not contain any displays.")
  dispfile <- paste0("displayInfo.json", ifelse(jsonp, "p", ""))
  ff <- list.files(file.path(app_path, "displays"), full.names = TRUE)
  idx <- which(unlist(lapply(ff, function(f)
    dir.exists(f) && file.exists(file.path(f, dispfile)))))
  lst <- lapply(ff[idx], function(f) {
    cur <- read_json_p(file.path(f, dispfile))
    cur[c("name", "description", "tags")]
  })
  txt <- get_jsonp_text(jsonp, paste0("__loadDisplayList__", id))
  cat(paste0(txt$st, as.character(to_json(lst, pretty = TRUE)), txt$nd),
    file = file.path(app_path, "displays",
      paste0("displayList", ifelse(jsonp, ".jsonp", ".json"))))
}
