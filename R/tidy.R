#' Apply a function to each element of a vector and return a vector of plots
#'
#' @param .x a list or atomic vector (see [`purrr::map()`] for details)
#' @param .f a function, formula, or atomic vector (see [`purrr::map()`] for
#' details)
#' @param ... additional arguments passed on to .f (see [`purrr::map()`] for
#' details)
#' @details See [`purrr::map()`]
#' @examples
#' \donttest{
#' library(dplyr)
#' library(tidyr)
#' library(purrr)
#'
#' x <- ggplot2::mpg |>
#'   tidyr::nest(data = !dplyr::one_of(c("manufacturer", "class"))) |>
#'   dplyr::mutate(panel = map_plot(data, function(x) {
#'     ggplot2::qplot(hwy, cty, data = x)
#'   })) |>
#'   trelliscope(name = "test")
#' }
#' @importFrom purrr map
#' @export
map_plot <- function(.x, .f, ...) {
  structure(
    purrr::map(.x, .f, ...),
    class = c("trelliscope_panels", "list")
  )
}

# #' Map over multiple inputs simultaneously and return a vector of plots
# #'
# #' @param .x,.y Vectors of the same length. A vector of length 1 will be
# #' recycled.
# #' @param .f A function, formula, or atomic vector (see purrr::map2() for
# #' details)
# #' @param ... additional arguments passed on to .f.
# #' @param .l A list of lists. The length of .l determines the number of
# #' arguments that .f will be called with. List names will be used if present.
# #' @details See purrr::map2()
# #' @examples
# #' \donttest{
# #' library(tidyr)
# #' library(purrr)
# #' library(plotly)
# #' library(dplyr)
# #'
# #' iris %>%
# #'   nest(data = -Species) %>%
# #'   mutate(
# #'     mod = map(data, ~ lm(Sepal.Length ~ Sepal.Width, data = .x)),
# #'     panel = map2_plot(data, mod, function(data, mod) {
# #'       plot_ly(data = data, x = ~Sepal.Width, y = ~Sepal.Length,
# #'         type = "scatter", mode = "markers", name = "data") %>%
# #'         add_trace(data = data, x = ~Sepal.Width, y = ~predict(mod),
# #'           mode = "lines", name = "lm")
# #'     })) %>%
# #'   trelliscope(name = "iris")
# #' }
# #' @export
# map2_plot <- function(.x, .y, .f, ...) {
#   structure(
#     purrr::map2(.x, .y, .f, ...),
#     class = c("trelliscope_panels", "list")
#   )
# }

# #' @export
# #' @rdname map2_plot
# pmap_plot <- function(.l, .f, ...) {
#   structure(
#     purrr::pmap(.l, .f, ...),
#     class = c("trelliscope_panels", "list")
#   )
# }

# #' @export
# `[.trelliscope_panels` <- function(x, i, j, ..., drop = TRUE) {
#   cls <- class(x)
#   x <- NextMethod()
#   structure(x, class = cls)
# }

# #' @export
# `[.panel_promise` <- function(x, i, j, ..., drop = TRUE) {
#   cls <- class(x)
#   x <- NextMethod()
#   structure(x, class = cls)
# }
