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
