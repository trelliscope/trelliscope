add_input_class <- function(x) {
  class(x) <- c("R6", "trelliscope_input_def")
  x
}

#' Specify a "numeric" input box
#' @param name Name of the input.
#' @param label Description of the input.
#' @param active Should the input be active by default?
#' @param min Optional minimum value to allow in the input.
#' @param max Optional maximum value to allow in the input.
#' @family {input types}
#' @export
input_number <- function(
  name, label = name, active = TRUE, min = NULL, max = NULL
) {
  NumberInput$new(
    name = name, label = label, active = active, min = min, max = max
  ) |>
    add_input_class()
}

#' Specify a "radio button" input
#' @inheritParams input_number
#' @param options A vector of radio button options.
#' @family {input types}
#' @export
input_radio <- function(
  name, label = name, active = TRUE, options
) {
  RadioInput$new(
    name = name, label = label, active = active, options = options
  ) |>
    add_input_class()
}

#' Specify a "checkbox" input
#' @inheritParams input_number
#' @param options A vector of checkbox options.
#' @family {input types}
#' @export
input_checkbox <- function(
  name, label = name, active = TRUE, options
) {
  CheckboxInput$new(
    name = name, label = label, active = active, options = options
  ) |>
    add_input_class()
}

#' Specify a "select" input
#' @inheritParams input_number
#' @param options A vector of options for the select dropdown.
#' @family {input types}
#' @export
input_select <- function(
  name, label = name, active = TRUE, options
) {
  SelectInput$new(
    name = name, label = label, active = active, options = options
  ) |>
    add_input_class()
}

#' Specify a "multiselect" input
#' @inheritParams input_number
#' @param options A vector of options for the multiselect dropdown.
#' @family {input types}
#' @export
input_multiselect <- function(
  name, label = name, active = TRUE, options
) {
  MultiselectInput$new(
    name = name, label = label, active = active, options = options
  ) |>
    add_input_class()
}

#' Specify a "text box" input
#' @inheritParams input_number
#' @param width Width (in characters) of the text box input.
#' @param height Height (in lines of text) of the text box input.
#' @family {input types}
#' @export
input_text <- function(
  name, label = name, active = TRUE, width = 80, height = 3
) {
  TextInput$new(
    name = name, label = label, active = active, width = width, height = height
  ) |>
    add_input_class()
}
