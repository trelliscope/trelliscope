panel_classes <- c("ggpanel_vec", "panel_lazy_vec", "panel_local_vec",
  "panel_url_vec")

panel_lazy_classes <- c("ggpanel_vec", "panel_lazy_vec")

panel_static_classes <- c("panel_local_vec", "panel_url_vec")

#' Set panel options for a Trelliscope display
#' @param trdf A trelliscope data frame created with [`as_trelliscope_df()`]
#' @param ... A named set panel options to set. The names should
#'  correspond to the names of the variables in the data frame. The values
#'  should come from [`panel_options()`] or [`panel_options_lazy()`].
#' @export
#' @examples
#' mars_rover |>
#'   as_trelliscope_df(name = "mars rover") |>
#'   set_panel_options(img_src = panel_options(aspect = 0.5))
set_panel_options <- function(trdf, ...) {
  trobj <- attr(trdf, "trelliscope")$clone()
  objs <- rlang::dots_list(...)
  nms <- names(trdf)
  for (nm in names(objs)) {
    assert(nm %in% nms,
      msg = "{.val {nm}} not found in the data frame - cannot set panel \\
        options")
    if (inherits(trdf[[nm]], panel_lazy_classes)) {
      opts_type <- "panel_options_lazy"
      if (is.null(objs[[nm]]$format)) {
        if (inherits(trdf[[nm]], "panel_lazy_vec")) {
          if (attr(trdf[[nm]], "type") == "htmlwidget") {
            objs[[nm]]$format <- "html"
          } else {
            objs[[nm]]$format <- "png"
          }
        } else {
          if (attr(trdf[[nm]], "as_plotly")) {
            objs[[nm]]$format <- "html"
          } else {
            objs[[nm]]$format <- "png"
          }
        }
      }
      if (objs[[nm]]$format == "html") {
        objs[[nm]]$type <- "iframe"
      } else {
        objs[[nm]]$type <- "img"
      }
    } else {
      opts_type <- "panel_options"
      exts <- tolower(unique(tools::file_ext(trdf[[nm]])))
      if (all(exts %in% valid_img_exts)) {
        objs[[nm]]$type <- "img"
      } else {
        objs[[nm]]$type <- "iframe"
      }
    }
    assert(inherits(objs[[nm]], c("panel_options", "panel_options_lazy")),
      msg = "panel options for {.val {nm}} must be specified using {.fn \\
        {opts_type}()}")

    trobj$panel_options[[nm]] <- objs[[nm]]
  }
  attr(trdf, "trelliscope") <- trobj
  trdf
}

#' Specify options for lazily-rendered panels in a Trelliscope display
#' @param width Width in pixels of each panel.
#' @param height Height in pixels of each panel.
#' @param format The format of the panels the server will provide. Can be
#'   one of "png" , "svg", or "html".
#' @param force Should server force panels to be written? If `FALSE`, if the
#'   panel has already been generated, that is what will be made available.
#' @param prerender If "TRUE", lazy panels will be rendered prior to viewing
#'   the display. If "FALSE", a local R websockets server will be created and
#'   plots will be rendered on the fly when requested by the app. The latter
#'   is only available when using Trelliscope locally.
#' @export
#' @examples
#' # TODO
panel_options_lazy <- function(
  width = 500, height = 500, format = NULL, force = FALSE, prerender = TRUE
) {
  assert(is.numeric(width) && length(width) == 1 && width > 0,
    msg = "width must be a single positive numeric value")
  assert(is.numeric(height) && length(height) == 1 && height > 0,
    msg = "height must be a single positive numeric value")
  assert(is.logical(force) && length(force) == 1,
    msg = "force must be a single logical value")
  assert(is.logical(prerender) && length(prerender) == 1,
    msg = "prerender must be a single logical value")

  if (!is.null(format)) {
    assert(is.character(format) && length(format) == 1,
      msg = "format must be a single character value")
    assert(format %in% c("png", "svg", "html"),
      msg = "format must be one of 'png', 'svg', or 'html'")
  }

  structure(list(width = width, height = height, prerender = prerender,
    force = force, format = format), class = "panel_options_lazy")
}

#' Specify options for lazily-rendered panels in a Trelliscope display
#' @param aspect The aspect ratio of the panel. This is the height divided
#'  by the width and specifies the relative dimension of the image in viewer.
#' @examples
#' mars_rover |>
#'   as_trelliscope_df(name = "mars rover") |>
#'   set_panel_options(img_src = panel_options(aspect = 0.5))
#' @export
panel_options <- function(aspect = 1) {
  assert(is.numeric(aspect) && length(aspect) == 1,
    msg = "aspect must be a single numeric value")
  assert(aspect > 0, msg = "aspect must be positive")
  structure(list(aspect = aspect), class = "panel_options")
}
