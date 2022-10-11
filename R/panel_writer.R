# # Write a list of plot objects as panels in a Trelliscope display
# # @param plot_list a named list of plot objects to be written as panels (objects can be trellis, ggplot2, or htmlwidget) with the list names being the keys for the panels
# # @param pb optional progress bar object to pass in and use to report progress
# # @param ... params passed directly to \code{\link{write_panel}}
# #' @import progress
# write_panels <- function(plot_list, ..., pb = NULL) {
#   nms <- names(plot_list)
#   assertthat::assert_that(length(nms) > 0,
#     msg = paste0("Panels must be a named list, with the names being used as ",
#       "the panel key"))

#   if (is.null(pb))
#     pb <- progress::progress_bar$new(
#       total = length(nms), width = getOption("width") - 5,
#       format = ":what [:bar] :percent :current/:total eta::eta")

#   lapply(nms, function(nm) {
#     pb$tick(tokens = list(what = "writing panels      "))
#     write_panel(plot_list[[nm]], key = nm, ...)
#   })

#   invisible(NULL)
# }

# # Write a plot object as a panel in a Trelliscope display
# # @param plot_object a plot object to be written (can be trellis, ggplot2,
# # or htmlwidget)
# # @param key a string identifying the panel key, which will be used as the
# # panel file name and which the \code{panelKey} column of the cognostics data
# # frame should point to
# # @param base_path the base directory of the trelliscope application
# # @param name name of the display that the panel belongs to
# # @param width width in pixels of each panel
# # @param height height in pixels of each panel
# # @param jsonp should json for panel be jsonp (TRUE) or json (FALSE)?
# # @template param-split-layout
# write_panel <- function(plot_object, key, base_path, name,
#   width, height, jsonp = TRUE, split_layout = FALSE) {

#   panel_path <- file.path(base_path, "displays", name,
#     ifelse(jsonp, "jsonp", "json"))

#   if (!dir.exists(panel_path)) {
#     res <- dir.create(panel_path, recursive = TRUE)
#     if (!res) {
#       warning("There was an issue creating directory to store panels in: ",
#       panel_path, ".  Panel will not be created.")
#       return(invisible(NULL))
#     }
#   }

#   if (inherits(plot_object, "ggplot") && split_layout) {
#     pg <- plot_gtable(plot_object)
#     left_axis <- extract_axis_left(pg = pg)
#     bottom_axis <- extract_axis_bottom(pg = pg)
#     plot_content <- extract_plot_content(pg = pg)

#     write_raster(
#       plot_content, width = width, height = height, key = paste0(key, "_plot"),
#       name = name, jsonp, panel_path
#     )
#     write_raster(
#       left_axis,
#       width = axis_left_width(left_axis),
#       height = height, key = paste0(key, "_axis_left"), name = name,
#       jsonp, panel_path
#     )
#     write_raster(
#       bottom_axis, width = width,
#       height = axis_bottom_height(bottom_axis),
#       key = paste0(key, "_axis_bottom"), name = name,
#       jsonp, panel_path
#     )

#     gg_legend <- extract_legend(pg = pg)
#     if (!is.null(gg_legend)) {

#       legend_width <- legend_width_or_height(gg_legend, "widths", width)
#       legend_height <- legend_width_or_height(gg_legend, "heights", height)

#       write_raster(
#         gg_legend,
#         width = legend_width,
#         height = legend_height,
#         key = paste0(key, "_legend"),
#         name = name,
#         jsonp, panel_path
#       )
#     }
#   } else if (inherits(plot_object, "htmlwidget")) {
#     write_htmlwidget(plot_object, width, height, key, name, jsonp, panel_path)
#   } else {
#     write_raster(plot_object, width, height, key, name, jsonp, panel_path)
#   }
# }

# unit_to_px <- function(x, res) {
#   ret <- unclass(grid::convertWidth(x, unitTo = "cm")) * res / 2.54
#   attr(ret, "unit") <- NULL
#   attr(ret, "valid.unit") <- NULL
#   ret
# }

# write_htmlwidget <- function(
#   plot_object, width, height, key, name, jsonp, panel_path
# ) {
#   p <- htmltools::as.tags(plot_object)
#   txt <- get_jsonp_text(jsonp, paste0("__panel__._", key, "_", name))
#   cat(paste0(txt$st, p[[2]]$children[[1]], txt$nd),
#     file = file.path(panel_path,
#       paste0(key, ifelse(jsonp, ".jsonp", ".json"))))
# }

# write_raster <- function(
#   plot_component, width, height, key, name, jsonp, panel_path,
#   file = tempfile()
# ) {
#   ff <- file
#   make_png(p = plot_component, file = ff,
#     width = width, height = height)
#   dat <- paste0("\"", encode_png(ff), "\"")
#   txt <- get_jsonp_text(jsonp, paste0("__panel__._", key, "_", name))
#   cat(paste0(txt$st, dat, txt$nd),
#     file = file.path(panel_path,
#       paste0(key, ifelse(jsonp, ".jsonp", ".json"))))
# }

# #' @importFrom base64enc base64encode
# encode_png <- function(file) {
#   paste("data:image/png;base64,",
#     base64enc::base64encode(
#       readBin(file, "raw", n = file.info(file)$size)),
#     sep = "")
# }

# #' @importFrom htmltools as.tags htmlDependencies
# get_dependencies <- function(widget_object) {
#   if (inherits(widget_object, "htmlwidget")) {
#     pt <- htmltools::as.tags(widget_object)
#     deps <- htmltools::htmlDependencies(pt)
#   } else {
#     deps <- NULL
#   }
#   deps
# }

# #' @import htmltools
# get_and_write_widget_deps <- function(
#   widget_object, base_path, self_contained
# ) {
#   if (!inherits(widget_object, "htmlwidget")) {
#     # message("object is not an htmlwidget so there are no widgets to write")
#     return(invisible(NULL))
#   }

#   if (self_contained) {
#     return(list(name = class(widget_object)[1]))
#   } else {
#     deps <- get_dependencies(widget_object)

#     libdir <- file.path(base_path, "lib")
#     dir.create(base_path, recursive = TRUE, showWarnings = FALSE)
#     oldwd <- getwd()
#     on.exit(setwd(oldwd), add = TRUE)
#     setwd(base_path)
#     dir.create(libdir, recursive = TRUE, showWarnings = FALSE)

#     deps2 <- do.call(c, lapply(deps, function(x) {
#       x <- htmltools::copyDependencyToDir(x, libdir, FALSE)
#       x <- htmltools::makeDependencyRelative(x, base_path, FALSE)
#       res <- list()
#       if (!is.null(x$script)) {
#         res[[length(res) + 1]] <- list(type = "script",
#           url = paste(x$src, x$script, sep = "/"))
#       }
#       if (!is.null(x$stylesheet)) {
#         res[[length(res) + 1]] <- list(type = "stylesheet",
#           url = paste(x$src, x$stylesheet, sep = "/"))
#       }
#       res
#     }))

#     return(list(name = class(widget_object)[1], assets = deps2))
#   }
# }

# get_png_units <- function(width, height, orig_width = width, res = 72,
#   base_point_size = 12, pixelratio = 2) {

#   width <- as.numeric(gsub("px", "", width))
#   height <- as.numeric(gsub("px", "", height))
#   orig_width <- as.numeric(gsub("px", "", orig_width))

#   # need to convert unit to pixels
#   # need to have 'fac' factor if coming from unit
#   width_is_unit <- height_is_unit <- FALSE
#   if (inherits(width, "unit")) {
#     width <- unit_to_px(width, res)
#     orig_width <- width
#     width_is_unit <- TRUE
#   }
#   if (inherits(height, "unit")) {
#     height <- unit_to_px(height, res)
#     # orig_height <- height
#     height_is_unit <- TRUE
#   }

#   fac <- max(min(width / orig_width, 1), 0.65) * 1.5

#   list(
#     res = res * pixelratio * fac,
#     width = width * pixelratio * ifelse(width_is_unit, fac, 1),
#     height = height * pixelratio * ifelse(height_is_unit, fac, 1),
#     pointsize = base_point_size * fac
#   )
# }

# #' @importFrom grDevices png
# make_png <- function(p, file, width, height, orig_width = width, res = 72,
#   base_point_size = 12, pixelratio = 2) {

#   pngfun <- grDevices::png

#   units <- get_png_units(width, height, orig_width, res,
#     base_point_size, pixelratio)

#   pngfun(filename = file,
#     res = units$res,
#     width = units$width,
#     height = units$height,
#     pointsize = units$pointsize)

#   unknown_object <- FALSE
#   dv <- grDevices::dev.cur()
#   tryCatch({
#     if (inherits(p, "trellis")) {
#       # p$par.settings$fontsize <-
#       #   list(text = pointsize, points = pointsize * 2 / 3)
#       print(p)
#     } else if (inherits(p, "ggplot")) {
#       print(p)
#     } else if (inherits(p, "gtable")) {
#       grid::grid.draw(p)
#     } else {
#       unknown_object <- TRUE
#       try(print(p), silent = TRUE)
#     }
#   },
#   finally = grDevices::dev.off(dv))

#   # if panel function didn't plot anything then make a blank panel
#   # res = res * pixelratio,
#   if (!file.exists(file)) {
#     if (unknown_object) {
#       cls <- paste(class(p), collapse = ", ")
#       message("The panel object of class'", cls,
#         "' is not a standard plot object and did not produce a panel file.")
#     }
#     pngfun(filename = file, width = width * pixelratio,
#       height = height * pixelratio, pointsize = units$pointsize)
#     blank_image("no panel")
#     grDevices::dev.off()
#   }
# }
