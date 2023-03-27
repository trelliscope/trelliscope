# Write a plot object as a panel in a Trelliscope display
# @param plot_object a plot object to be written (can be trellis, ggplot2,
# or htmlwidget)
# @param key a string identifying the panel key, which will be used as the
# panel file name and which the \code{panelKey} column of the cognostics data
# frame should point to
# @param base_path the base directory of the trelliscope application
# @param name name of the display that the panel belongs to
# @param width width in pixels of each panel
# @param height height in pixels of each panel
write_panel <- function(x, key, base_path, panel_path,
  width, height, format = "png", html_head = NULL) {
  if (inherits(x, "htmlwidget")) {
    write_htmlwidget(x, key, panel_path, html_head)
  } else {
    file <- file.path(panel_path, paste0(key, ".", format))
    if (format == "png") {
      make_png(p = x, file = file, width = width, height = height)
    } else if (format == "svg") {
      make_svg(p = x, file, width, height)
    }
  }
}

#' @importFrom htmlwidgets saveWidget
write_htmlwidget_deps <- function(x, base_path, panel_path) {
  deps_path <- file.path(base_path, "libs")
  widget_name <- class(x)[1]

  htmlwidgets::saveWidget(x, file.path(deps_path, "index.html"),
    libdir = widget_name, selfcontained = FALSE)

  html_head <- readLines(file.path(deps_path, "index.html"))
  idx <- which(grepl("<body>", html_head))
  assert(length(idx) > 0, "problem...")
  html_head <- html_head[1:idx]
  html_head <- gsub("src=\"", "src=\"../../../libs/", html_head)
  html_head <- gsub("href=\"", "href=\"../../../libs/", html_head)

  unlink(file.path(base_path, "libs", "index.html"))

  html_head
}

#' @importFrom htmltools as.tags
write_htmlwidget <- function(x, key, panel_path, html_head) {
  x$sizingPolicy <- htmlwidgets::sizingPolicy(
    defaultWidth = "100vw", defaultHeight = "100vh", padding = 0)
  p <- htmltools::as.tags(x, standalone = TRUE)

  html <- paste0(
    paste(html_head, collapse = "\n"),
    as.character(p),
    paste("</body>\n</html>")
  )
  cat(html, file = file.path(panel_path, paste0(key, ".html")))
}

unit_to_px <- function(x, res) {
  ret <- unclass(grid::convertWidth(x, unitTo = "cm")) * res / 2.54
  attr(ret, "unit") <- NULL
  attr(ret, "valid.unit") <- NULL
  ret
}

get_png_units <- function(width, height, orig_width = width, res = 72,
  base_point_size = 12, pixelratio = 2) {

  width <- as.numeric(gsub("px", "", width))
  height <- as.numeric(gsub("px", "", height))
  orig_width <- as.numeric(gsub("px", "", orig_width))

  # need to convert unit to pixels
  # need to have 'fac' factor if coming from unit
  width_is_unit <- height_is_unit <- FALSE
  if (inherits(width, "unit")) {
    width <- unit_to_px(width, res)
    orig_width <- width
    width_is_unit <- TRUE
  }
  if (inherits(height, "unit")) {
    height <- unit_to_px(height, res)
    height_is_unit <- TRUE
  }

  fac <- max(min(width / orig_width, 1), 0.65) * 1.5

  list(
    res = res * pixelratio * fac,
    width = width * pixelratio * ifelse(width_is_unit, fac, 1),
    height = height * pixelratio * ifelse(height_is_unit, fac, 1),
    pointsize = base_point_size * fac
  )
}

#' @importFrom grDevices png dev.cur dev.off
#' @importFrom grid grid.draw
make_png <- function(p, file, width, height, orig_width = width, res = 72,
  base_point_size = 12, pixelratio = 2) {

  pngfun <- grDevices::png

  units <- get_png_units(width, height, orig_width, res,
    base_point_size, pixelratio)

  pngfun(filename = file,
    res = units$res,
    width = units$width,
    height = units$height,
    pointsize = units$pointsize)

  dv <- grDevices::dev.cur()
  tryCatch({
    if (inherits(p, c("trellis", "ggplot"))) {
      # p$par.settings$fontsize <-
      #   list(text = pointsize, points = pointsize * 2 / 3)
      print(p)
    } else if (inherits(p, "gtable")) {
      grid::grid.draw(p)
    } else {
      if (file.exists(file))
        unlink(file)
      try(capture.output(print(p)), silent = TRUE)
      # if panel function didn't plot anything then make a blank panel
      # res = res * pixelratio,
      if (!file.exists(file)) {
        msg("The panel object of class '{class(p)}' is not a standard \\
        plot object and did not produce a panel file.")
        print(blank_image("no panel"))
      }
    }
  },
  finally = grDevices::dev.off(dv))
}

#' @importFrom svglite svglite
make_svg <- function(p, file, width, height) {
  svglite::svglite(filename = file,
    width = width / 72, height = height / 72, scaling = 2)
  print(p)
  dev.off()
}

#' @importFrom ggplot2 aes element_blank element_rect geom_text ggplot labs
#' scale_y_continuous theme unit
blank_image <- function(txt = "no thumbnail") {
  dd <- data.frame(x = 0.5, y = 0.75, label = txt)
  ggplot2::ggplot() +
    ggplot2::geom_text(ggplot2::aes(
      x = dd$x, y = dd$y, label = dd$label), size = 8) +
    ggplot2::labs(x = NULL, y = NULL, title = NULL) +
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = "transparent", colour = NA),
      plot.background = ggplot2::element_rect(
        fill = "transparent", colour = NA),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      plot.margin = ggplot2::unit(c(0, 0, 0, 0), "null"),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      legend.position = "none",
      axis.ticks.length = ggplot2::unit(0, "null")
    )
}
