#' View a trelliscope display
#' @param trdf A trelliscope data frame created with [`as_trelliscope_df()`]
#' or a data frame which will be cast as such.
#' @examples
#' library(ggplot2)
#'
#' panel_dat <- (
#'   ggplot(gapminder, aes(year, lifeExp)) +
#'     geom_point() +
#'     facet_panels(vars(country, continent))
#' ) |>
#'   as_panels_df()
#'
#' disp <- panel_dat |>
#'   as_trelliscope_df(name = "life_expectancy")
#'
#' \dontrun{
#' view_trelliscope(disp)
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
    # pth <- file.path(trobj$get_display_path(),
    #   paste0("displayInfo.", c("json", "jsonp")))
    # if (!any(file.exists(pth))) {
    #   msg("Display has not been written... writing...")

    on.exit({
      options(trelliscope_view_trelliscope = FALSE)
    }, add = TRUE)
    options(trelliscope_view_trelliscope = TRUE)
    trdf <- write_trelliscope(trdf)
    options(trelliscope_view_trelliscope = FALSE)

    has_server <- requires_server(trdf)
    if (has_server)
      wrn("Trelliscope displays that have panels with `prerender = FALSE` \\
        cannot be viewed in an RMarkdown document.")
    # }
    cur_opts <- knitr::opts_current$get()
    # url <- paste0(trobj$path, "/index.html")
    # TODO: enforce minimum width
    width <- cur_opts$out.width.px
    height <- cur_opts$out.height.px
    title <- trobj$get("name")

    cur_opts <- knitr::opts_current$get()
    url <- paste0(trobj$path, "/index.html")
    # fix for pkgdown github action:
    if (grepl("/home/runner/work/.*/docs/articles/", url))
      url <- gsub("/home/runner/work/.*/docs/articles/(.*)", "\\1", url)
    scale <- 1
    if (!is.null(cur_opts$scale))
      scale <- as.numeric(cur_opts$scale)
    trueWidth <- cur_opts$out.width.px
    width <- scale_w_suffix(trueWidth, scale)
    trueHeight <- cur_opts$out.height.px
    height <- scale_w_suffix(trueHeight, scale)
    title <- trobj$get("name")

    # NOTE: iframe tag has to be on same line as div tag or it doesn't work
    iframe <- glue::glue("
      <div style=\"width: {trueWidth}; height: {trueHeight}; padding-bottom: 5px;\"><iframe
          style=\"
            transform: scale({scale});
            width: {width};
            height: {height};
            margin: 0;
            padding: 0;
            border: 1px solid #efefef;
            transform-origin: top left;
          \"
          src=\"{url}\"
          title=\"{title}\"
          width=\"{width}px\"
          height=\"{height}px\"
          allowfullscreen
          style=\"margin: 0; padding: 0; border: 1px solid #efefef;\"
        >
        </iframe></div>
    ")
    return(knitr::asis_output(iframe))
  }

  has_server <- FALSE
  if (is.null(trdf)) {
    url <- getOption("trelliscope_latest_display_url")
    if (is.null(url)) {
      msg("Cannot view. A trelliscope object was not provided.")
      return(NULL)
    }
  } else {
    trdf <- check_trelliscope_df(trdf)
    trobj <- attr(trdf, "trelliscope")
    # pth <- file.path(trobj$get_display_path(),
    #   paste0("displayInfo.", c("json", "jsonp")))
    # if (!any(file.exists(pth))) {
    #   msg("Display has not been written... writing...")
    trdf <- write_trelliscope(trdf)
    # }
    has_server <- requires_server(trdf)
    url <- file.path(trobj$path, "index.html")
  }

  options(trelliscope_latest_display_url = url)

  get_viewer()(url)

  if (has_server) {
    start_server(trdf)
  }
}

requires_server <- function(trdf) {
  trobj <- attr(trdf, "trelliscope")
  idx <- which(unlist(lapply(trobj$get("metas"), function(x)
    inherits(x, "PanelMeta") &&
    inherits(x$get("source"), "LocalWebSocketPanelSource")
  )))
  length(idx) > 0
}
