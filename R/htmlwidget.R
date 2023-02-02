
view_display <- function(
  obj, spa = TRUE, width = NULL, height = NULL
) {
  path <- obj$path
  config_info <- list.files(path, pattern = "config.json")
  id <- readLines(file.path(path, "id"), warn = FALSE)[1]

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

  fidelius_pars <- obj$fidelius_pars
  if (!is.null(fidelius_pars)) {
    rlang::check_installed("fidelius",
      reason = "to encrypt your Trelliscope display.")
    fidelius_pars$input <- index_html
    do.call(fidelius::charm, fidelius_pars)
  }

  viewer <- get_viewer(wdgt)
  if (!is.null(viewer))
    viewer(index_html)

  invisible(index_html)
}

get_viewer <- function(wdgt) {
  viewer <- getOption("viewer")
  if (!is.null(viewer)) {
    viewerFunc <- function(url) {

      # get the requested pane height (it defaults to NULL)
      paneHeight <- wdgt$sizingPolicy$viewer$paneHeight

      # convert maximize to -1 for compatibility with older versions of rstudio
      # (newer versions convert 'maximize' to -1 interally, older versions
      # will simply ignore the height if it's less than zero)
      if (identical(paneHeight, "maximize"))
        paneHeight <- -1

      # call the viewer
      viewer(url, height = paneHeight)
    }
  } else {
    viewerFunc <- utils::browseURL
  }
  viewerFunc
}

# nolint start

#' Shiny bindings for trelliscope
#'
#' Output and render functions for using trelliscope within Shiny
#' applications and interactive Rmd documents.
#'
#' @param output_id output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a trelliscope
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name trelliscope-shiny
#'
#' @export
trelliscopeOutput <- function(output_id, width = "100%", height = "400px") {
  htmlwidgets::shinyWidgetOutput(output_id, "trelliscope", width, height,
    package = "trelliscope")
}

#' @rdname trelliscope-shiny
#' @export
renderTrelliscope <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  }
  htmlwidgets::shinyRenderWidget(expr, trelliscopeOutput, env, quoted = TRUE)
}

# nolint end
