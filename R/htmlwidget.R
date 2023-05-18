#' View a trelliscope display
#' @param trdf A trelliscope data frame created with [`as_trelliscope_df()`]
#' or a data frame which will be cast as such.
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' panel_dat <- (
#'   ggplot(gapminder, aes(year, lifeExp)) +
#'     geom_point() +
#'     facet_panels(~country + continent)
#' ) |>
#'   nest_panels()
#'
#' disp <- panel_dat |>
#'   as_trelliscope_df(name = "life_expectancy") |>
#'   write_panels() |>
#'   write_trelliscope() |>
#'   view_trelliscope()
#'
#' # Alternatively you can build your trelliscope and call `view_trelliscope()`
#' # separately. This allows for fine tuning of the trelliscope without having
#' # to reopen it every time you make an edit.
#' trell <- (
#'   ggplot(gapminder, aes(year, lifeExp)) +
#'     geom_point() +
#'     facet_panels(~country + continent)
#' ) |>
#'   nest_panels() |>
#'   as_trelliscope_df(name = "life_expectancy") |>
#'   write_panels() |>
#'   write_trelliscope()
#'
#' view_trelliscope(trell)
#' }
#' @export
view_trelliscope <- function(trdf = NULL) {
  if (in_rmarkdown()) {
    if (!can_print_rmarkdown()) {
      wrn("Trelliscope should only be rendered in RMarkdown when the output \
        format is 'html_document' and self_contained is false. Trelliscope \
        always produces auxiliary files. Consider publishing and embedding the \
        published display in an iframe instead.")
      return(invisible(NULL))
    }
    trobj <- attr(trdf, "trelliscope")
    pth <- file.path(trobj$get_display_path(),
      paste0("displayInfo.", c("json", "jsonp")))
    if (!any(file.exists(pth))) {
      msg("Display has not been written... writing...")
      write_trelliscope(trdf)
    }
    cur_opts <- knitr::opts_current$get()
    url <- paste0(trobj$path, "/index.html")
    # TODO: enforce minimum width
    width <- cur_opts$out.width.px
    height <- cur_opts$out.height.px
    title <- trobj$get("name")
    iframe <- glue::glue("
      <iframe
        src=\"{url}\"
        title=\"{title}\"
        width=\"{width}px\"
        height=\"{height}px\"
        allowfullscreen
        style=\"margin: 0; padding: 0; border: 1px solid #efefef;\"
      >
      </iframe>
    ")
    return(knitr::asis_output(iframe))
  }

  if (is.null(trdf)) {
    url <- getOption("trelliscope_latest_display_url")
    if (is.null(url)) {
      msg("Cannot view. A trelliscope object was not provided.")
      return(NULL)
    }
  } else {
    trdf <- check_trelliscope_df(trdf)
    trobj <- attr(trdf, "trelliscope")
    pth <- file.path(trobj$get_display_path(),
      paste0("displayInfo.", c("json", "jsonp")))
    if (!any(file.exists(pth))) {
      msg("Display has not been written... writing...")
      write_trelliscope(trdf)
    }
    url <- file.path(trobj$path, "index.html")
  }

  options(trelliscope_latest_display_url = url)

  get_viewer()(url)
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
