scale_w_suffix <- function(val, scale) {
  # regex to get all non-numeric characters at end of string
  suffix <- gsub("[0-9]+", "", val)
  # remove suffix from val
  num <- suppressWarnings(as.numeric(gsub(suffix, "", val)))
  if (is.na(num))
    return(val)
  return(paste0(num / scale, suffix))
}


# spa = TRUE, width = NULL, height = NULL
write_widget <- function(trobj) {
  path <- trobj$path
  config_info <- list.files(path, pattern = "config.json")
  id <- readLines(file.path(path, "id"), warn = FALSE)[1]
  spa <- TRUE

  x <- list(
    id = id,
    config_info = config_info,
    spa = spa
  )

  if (spa) {
    width <- "100vw"
    height <- "100vh"
  }

  # create widget
  wdgt <- htmlwidgets::createWidget(
    name = "trelliscope_widget",
    x,
    width = width,
    height = height,
    package = "trelliscope",
    sizingPolicy = htmlwidgets::sizingPolicy(padding = 0, browser.fill = TRUE,
      knitr.defaultWidth = 900, knitr.defaultHeight = 550, knitr.figure = FALSE,
      viewer.defaultWidth = "100%", viewer.defaultHeight = "100%",
      viewer.padding = 0, viewer.fill = TRUE, browser.defaultWidth = "100%",
      browser.defaultHeight = "100%", browser.padding = 0)
  )

  index_html <- file.path(path, "index.html")

  el_tags <- htmltools::as.tags(wdgt, standalone = FALSE)
  htmltools::save_html(el_tags, file = index_html, libdir = "lib")

  fidelius_pars <- trobj$fidelius_pars
  if (!is.null(fidelius_pars)) {
    rlang::check_installed("fidelius",
      reason = "to encrypt your Trelliscope display.")
    fidelius_pars$input <- index_html
    do.call(fidelius::charm, fidelius_pars)
  }

  options(trelliscope_latest_display_url = index_html)
  msg("Trelliscope written to {index_html}
    Open this file or call view_trelliscope() to view.")

  invisible(index_html)
}

get_viewer <- function() {
  viewer <- getOption("viewer")
  if (is.null(viewer))
    viewer <- utils::browseURL
  viewer
}

# get_viewer <- function(wdgt) {
#   viewer <- getOption("viewer")
#   if (!is.null(viewer)) {
#     viewerFunc <- function(url) {

#       # get the requested pane height (it defaults to NULL)
#       paneHeight <- wdgt$sizingPolicy$viewer$paneHeight

#       # convert maximize to -1 for compatibility with older versions of rstudio
#       # (newer versions convert 'maximize' to -1 interally, older versions
#       # will simply ignore the height if it's less than zero)
#       if (identical(paneHeight, "maximize"))
#         paneHeight <- -1

#       # call the viewer
#       viewer(url, height = paneHeight)
#     }
#   } else {
#     viewerFunc <- utils::browseURL
#   }
#   viewerFunc
# }
