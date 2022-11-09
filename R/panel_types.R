#' Cast a vector of URLs pointing to images as an image panel source
#'
#' @param x a vector of URLs pointing to images
#' @export
img_panel <- function(x) {
  check_atomic(x, "img_panel variable")
  check_img_ext(x, "img_panel")
  class(x) <- c(class(x), "img_panel")
  x
}

#' Cast a vector of URLs pointing to local images as an image panel source
#'
#' @param x a vector of URLs pointing to images
#' @note \code{x} must be paths relative to the \code{path} argument passed to
#'   \code{\link{trelliscope}}.
#' @importFrom tools file_ext
#' @export
img_panel_local <- function(x) {
  check_atomic(x, "img_panel_local variable")
  check_img_ext(x, "img_panel_local")
  class(x) <- c(class(x), "img_panel", "img_panel_local")
  x
}

valid_img_exts <- c("apng", "avif", "gif", "jpg", "jpeg", "jfif", "pjpeg",
  "pjp", "png", "svg", "webp")

check_img_ext <- function(x, fn) {
  exts <- unique(tools::file_ext(x))
  assert(all(exts %in% valid_img_exts),
    msg = paste0("For ", fn, "(), all file extensions must be one of ",
      paste(valid_img_exts, collapse = ", ")))
  TRUE
}

#' Cast a vector of URLs pointing to html pages as an image panel source
#'
#' @param x a vector of URLs pointing to html pages
#' @export
iframe_panel <- function(x) {
  check_atomic(x, "iframe_panel variable")
  check_html_ext(x, "iframe_panel")
  class(x) <- c(class(x), "iframe_panel")
  x
}

#' Cast a vector of URLs pointing to html pages as an image panel source
#'
#' @param x a vector of URLs pointing to html pages
#' @note \code{x} must be paths relative to the \code{path} argument passed to
#'   \code{\link{trelliscope}}.
#' @export
iframe_panel_local <- function(x) {
  check_atomic(x, "iframe_panel_local variable")
  check_html_ext(x, "iframe_panel_local")
  class(x) <- c(class(x), "iframe_panel", "iframe_panel_local")
  x
}

check_html_ext <- function(x, fn) {
  exts <- unique(tools::file_ext(x))
  assert(all(exts == "html"),
    msg = paste0("For ", fn, "(), all file extensions must be .html"))
  TRUE
}
