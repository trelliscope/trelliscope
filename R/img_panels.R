#' Cast a vector of URLs pointing to images as an image panel source
#'
#' @param x a vector of URLs pointing to images
#' @export
img_panel <- function(x) {
  check_atomic(x, "img_panel variable")
  class(x) <- c(class(x), "img_panel")
  x
}

#' Cast a vector of URLs pointing to local images as an image panel source
#'
#' @param x a vector of URLs pointing to images
#' @export
#' @note \code{x} must be paths relative to the \code{path} argument passed to \code{\link{trelliscope}}.
#' @examples
#' \dontrun{
#' # assuming images are available locally in relative path pokemon_local/images
#' pokemon$img <- img_panel_local(paste0("images/", basename(pokemon$url_image)))
#' trelliscope(pokemon, name = "pokemon", path = "pokemon_local")
#' }
img_panel_local <- function(x) {
  check_atomic(x, "img_panel_local variable")
  class(x) <- c(class(x), "img_panel", "img_panel_local")
  x
}
