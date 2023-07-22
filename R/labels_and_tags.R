#' Set labels for variables in a data frame
#' @param d A data frame.
#' @param ... A named set of labels, where each name must correspond to one
#'   of the variables in the dataset.
#' @examples
#' set_var_labels(mtcars, mpg = "Miles per gallon", cyl = "Number of cylinders")
#' @importFrom rlang dots_list
#' @export
set_var_labels <- function(d, ...) {
  assert(is.data.frame(d), msg = "`d` must be a data frame")
  vals <- rlang::dots_list(...)
  for (v in intersect(names(vals), names(d)))
    attr(d[[v]], "label") <- vals[[v]]
  d
}

#' Set tags for variables in a data frame
#' @param d A data frame.
#' @param ... A named set of vectors of variable names, where each name is a
#'   tag and each value in each vector corresponds to one of the variables in
#'   the dataset.
#' @examples
#' set_tags(iris,
#'   info = "Species",
#'   metrics = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
#'   width = c("Sepal.Width", "Petal.Width"))
#' @importFrom rlang dots_list
#' @importFrom tibble enframe deframe
#' @importFrom tidyr unnest
#' @importFrom dplyr group_by summarize .data
#' @export
set_tags <- function(d, ...) {
  assert(is.data.frame(d), msg = "`d` must be a data frame")
  vals <- rlang::dots_list(...)
  # invert vals to be a list of variable names with corresponding tags
  ivals <- vals |>
    tibble::enframe() |>
    tidyr::unnest(cols = dplyr::all_of("value")) |>
    dplyr::group_by(.data$value) |>
    dplyr::summarize(tags = list(.data$name)) |>
    tibble::deframe()

  for (v in intersect(names(ivals), names(d)))
    attr(d[[v]], "tags") <- ivals[[v]]

  d
}
