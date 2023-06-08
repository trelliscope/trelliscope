#' Add a plot column to a dataset
#' @param plot_fn A function that produces a panel from a given subset of `data`
#' @param data A data frame from which subsets will be extracted and plots will
#'   be made. Should be a superset of the summary dataset to which this plot
#'   column is being added.
#' @param by A list of variables found in both `data` and in the summary
#'   dataset to which this plot column is being added. This is used to specify
#'   which subset of `data` to apply for a given plot.
# #' @param width Width in pixels of each panel.
# #' @param height Height in pixels of each panel.
# #' @param format The format of the panels the server will provide. Can be
# #'   one of "png" , "svg", or "html".
# #' @param force Should server force panels to be written? If `FALSE`, if the
# #'   panel has already been generated, that is what will be made available.
# #' @param prerender If "TRUE", the plots must be rendered prior to viewing the
# #'   display. If "FALSE", a local R websockets server will be created and
# #'   plots will be rendered on the fly when requested by the app. The latter
# #'   is only available when using Trelliscope locally.
#' @export
panel_calc <- function(
  plot_fn, data, by = NULL, cur = dplyr::pick(dplyr::everything())
) {
  assert(is.function(plot_fn),
    msg = "`plot_fn` must be a function")
  assert(is.data.frame(data),
    msg = "`data`` must be a data frame")
  if (!is.null(by)) {
    assert(is.character(by),
      msg = "`by` must be a character vector")
    assert(all(by %in% names(data)),
      msg = "All elements of `by` must be found in `data`")
  }

  if (is.null(by)) {
    by <- intersect(names(data), names(cur))
    # TODO: should make sure these are atomic, etc.
  }

  # get "by" values for each row
  by_vals <- cur %>%
    dplyr::select(dplyr::all_of(by)) %>%
    split(seq_len(nrow(cur))) %>%
    unname() %>%
    lapply(function(x) {
      x <- as.list(x)
      lapply(x, function(a) {
        if (is.factor(a))
          a <- as.character(a)
        a
      })
    })

  # test plot function on a subset
  nd <- data
  for (gv in by)
    nd <- dplyr::filter(nd, .data[[gv]] == by_vals[[1]][[gv]][[1]])
  nd <- dplyr::collect(nd)
  p <- plot_fn(nd)
  if (inherits(p, "htmlwidget")) {
    type <- "htmlwidget"
  } else if (inherits(p, "ggplot")) {
    type <- "ggplot"
  } else {
    stop("plot_fn must return either an htmlwidget or a ggplot object")
  }

  vctrs::new_rcrd(
    fields = list(by = by_vals),
    plot_fn = plot_fn,
    by = by,
    data = data,
    type = type,
    class = "panel_calc_vec"
  )
}

get_panel <- function(x) {
  UseMethod("get_panel")
}

# only meant to work if x is a single element
#' @export
get_panel.panel_calc_vec <- function(x) {
  nd <- attr(x, "data")
  by <- attr(x, "by")
  plot_fn <- attr(x, "plot_fn")
  by_vals <- vctrs::vec_data(x)$by[[1]]
  for (gv in by)
    nd <- dplyr::filter(nd, .data[[gv]] == by_vals[[gv]][[1]])
  nd <- dplyr::collect(nd)
  plot_fn(nd)
}

#' @export
format.panel_calc_vec <- function(x, ...) {
  # vctrs::field(x, "path")
  if (length(x) == 1)
    print(get_panel(x))
  rep(paste0("<", attr(x, "type"), ">"), length(x))
}

#' @importFrom vctrs vec_ptype_abbr
#' @export
vec_ptype_abbr.panel_calc_vec <- function(x) {
  "panel"
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.panel_calc_vec <- function(x, ...) {
  out <- rep(paste0("<", attr(x, "type"), ">"), length(x))
  pillar::new_pillar_shaft_simple(out, align = "left")
}

# #' @export
# obj_print_data.panel_calc_vec <- function(x) {
#   cat(format(x), sep = "\n")
# }

#' @export
panel_url <- function(urls) {
  assert(is.character(urls),
    msg = "`urls` must be a character vector")

  vctrs::new_vctr(urls, class = "panel_url_vec")
}

# only meant to work if x is a single element
#' @export
get_panel.panel_url_vec <- function(x) {
  utils::browseURL(vctrs::vec_data(x))
}

#' @export
format.panel_url_vec <- function(x, ...) {
  if (length(x) == 1)
    print(get_panel(x))
  vctrs::vec_data(x)
}

#' @importFrom vctrs vec_ptype_abbr
#' @export
vec_ptype_abbr.panel_url_vec <- function(x) {
  "panel"
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.panel_url_vec <- function(x, ...) {
  out <- vctrs::vec_data(x)
  nc <- nchar(out)
  out_short <- substr(out, nc - 10, nc)
  pillar::new_pillar_shaft_simple(out, align = "left",
    width = 10, shorten = "front", short_formatted = out_short)
}


#' @export
panel_local <- function(paths) {
  assert(is.character(paths),
    msg = "`paths` must be a character vector")

  vctrs::new_vctr(paths, class = "panel_local_vec")
}

# only meant to work if x is a single element
#' @export
get_panel.panel_local_vec <- function(x) {
  # TODO: look for raster image file extensions instead
  if (tools::file_ext(vctrs::vec_data(x)) == "html") {
    utils::browseURL(vctrs::vec_data(x))
  } else {
    magick::image_read(vctrs::vec_data(x))
  }
}

#' @export
format.panel_local_vec <- function(x, ...) {
  if (length(x) == 1)
    print(get_panel(x), info = FALSE)
  vctrs::vec_data(x)
}

#' @importFrom vctrs vec_ptype_abbr
#' @export
vec_ptype_abbr.panel_local_vec <- function(x) {
  "panel"
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.panel_local_vec <- function(x, ...) {
  out <- vctrs::vec_data(x)
  pillar::new_pillar_shaft_simple(out, align = "left",
    width = 10, shorten = "front")
}




# width = 500, height = 500, format = NULL,
#   force = FALSE, prerender = TRUE

# TODO: get rid of the stuff below??

#' Cast a vector of URLs pointing to images as an image panel source
#'
#' @param x A vector of URLs pointing to images.
#' @param aspect_ratio Specifies the aspect ratio (width / height) to use when
#'  displaying the image in the browser.
#' @export
img_panel <- function(x, aspect_ratio = 1.5) {
  check_atomic(x, "img_panel")
  check_img_ext(x, "img_panel")
  check_scalar(aspect_ratio, "aspect_ratio")
  check_pos_numeric(aspect_ratio, "aspect_ratio")
  class(x) <- c(class(x), "img_panel")
  attr(x, "aspect_ratio") <- aspect_ratio
  x
}

#' Cast a vector of URLs pointing to local images as an image panel source
#'
#' @param x A vector of URLs pointing to images.
#' @param aspect_ratio Specifies the aspect ratio (width / height) to use when
#'  displaying the image in the browser.
#' @note \code{x} must be paths relative to the \code{path} argument passed to
#'   \code{\link{as_trelliscope_df}}.
#' @importFrom tools file_ext
#' @export
img_panel_local <- function(x, aspect_ratio = 1.5) {
  check_atomic(x, "img_panel_local")
  check_img_ext(x, "img_panel_local")
  check_scalar(aspect_ratio, "aspect_ratio")
  check_pos_numeric(aspect_ratio, "aspect_ratio")
  class(x) <- c(class(x), "img_panel", "img_panel_local")
  attr(x, "aspect_ratio") <- aspect_ratio
  x
}

valid_img_exts <- c("apng", "avif", "gif", "jpg", "jpeg", "jfif", "pjpeg",
  "pjp", "png", "svg", "webp")

check_img_ext <- function(x, fn) {
  exts <- tolower(unique(tools::file_ext(x)))
  assert(all(exts %in% valid_img_exts),
    msg = paste0("For ", fn, "(), all file extensions must be one of ",
      paste(valid_img_exts, collapse = ", ")))
  TRUE
}

#' Cast a vector of URLs pointing to html pages as an image panel source
#'
#' @param x A vector of URLs pointing to html pages.
#' @param aspect_ratio Specifies the aspect ratio (width / height) to use when
#'  displaying the image in the browser.
#' @export
iframe_panel <- function(x, aspect_ratio = 1.5) {
  check_atomic(x, "iframe_panel")
  # check_html_ext(x, "iframe_panel")
  check_scalar(aspect_ratio, "aspect_ratio")
  check_pos_numeric(aspect_ratio, "aspect_ratio")
  class(x) <- c(class(x), "iframe_panel")
  x
}

#' Cast a vector of URLs pointing to html pages as an image panel source
#'
#' @param x A vector of URLs pointing to html pages.
#' @param aspect_ratio Specifies the aspect ratio (width / height) to use when
#'  displaying the image in the browser.
#' @note \code{x} must be paths relative to the \code{path} argument passed to
#'   \code{\link{as_trelliscope_df}}.
#' @export
iframe_panel_local <- function(x, aspect_ratio = 1.5) {
  check_atomic(x, "iframe_panel_local")
  check_html_ext(x, "iframe_panel_local")
  check_scalar(aspect_ratio, "aspect_ratio")
  check_pos_numeric(aspect_ratio, "aspect_ratio")
  class(x) <- c(class(x), "iframe_panel", "iframe_panel_local")
  x
}

check_html_ext <- function(x, fn) {
  exts <- tolower(unique(tools::file_ext(x)))
  assert(all(exts == "html"),
    msg = paste0("For ", fn, "(), all file extensions must be .html"))
  TRUE
}
