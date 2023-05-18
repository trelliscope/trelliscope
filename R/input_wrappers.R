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
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' panel_dat <- (
#'   ggplot(gapminder, aes(year, lifeExp)) +
#'     geom_point() +
#'     facet_panels()
#'   ) |>
#'   nest_panels()
#'
#' trell <- panel_dat |>
#' as_trelliscope_df() |>
#' write_panels() |>
#' add_inputs(
#'   input_number(
#'     name = "Numeric Input",
#'     label = "A space to add custom ranking for sorting",
#'     min = 0, max = 10
#'   ),
#'   email = "johndoe@email.com"
#' ) |>
#' write_trelliscope() |>
#' view_trelliscope()
#' }
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
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' panel_dat <- (
#'   ggplot(gapminder, aes(year, lifeExp)) +
#'     geom_point() +
#'     facet_panels()
#'   ) |>
#'   nest_panels()
#'
#' trell <- panel_dat |>
#' as_trelliscope_df() |>
#' write_panels() |>
#' add_inputs(
#'   input_radio(
#'     name = "Radio Input",
#'     label = "A space to add custom ranking for sorting",
#'     options = c("yes", "no")
#'   ),
#'   email = "johndoe@email.com"
#' ) |>
#' write_trelliscope() |>
#' view_trelliscope()
#' }
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
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' panel_dat <- (
#'   ggplot(gapminder, aes(year, lifeExp)) +
#'     geom_point() +
#'     facet_panels()
#'   ) |>
#'   nest_panels()
#'
#' trell <- panel_dat |>
#' as_trelliscope_df() |>
#' write_panels() |>
#' add_inputs(
#'   input_checkbox(
#'     name = "Checkbox Input",
#'     label = "A space to add custom button inputs",
#'     options = c("yes", "no")
#'   )
#' ) |>
#' add_input_email("johndoe@email.com") |>
#' write_trelliscope() |>
#' view_trelliscope()
#' }
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
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' panel_dat <- (
#'   ggplot(gapminder, aes(year, lifeExp)) +
#'     geom_point() +
#'     facet_panels()
#'   ) |>
#'   nest_panels()
#'
#' trell <- panel_dat |>
#' as_trelliscope_df() |>
#' write_panels() |>
#' add_inputs(
#'   input_select(
#'     name = "Select Input",
#'     label = "A space to add custom dropdown inputs",
#'     options =c("yes", "no")
#'   ),
#'   email = "johndoe@email.com"
#' ) |>
#' write_trelliscope() |>
#' view_trelliscope()
#' }
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
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' panel_dat <- (
#'   ggplot(gapminder, aes(year, lifeExp)) +
#'     geom_point() +
#'     facet_panels()
#'   ) |>
#'   nest_panels()
#'
#' trell <- panel_dat |>
#' as_trelliscope_df() |>
#' write_panels() |>
#' add_inputs(
#'   input_multiselect(
#'     name = "Multiselect Input",
#'     label = "A space to add custom dropdown inputs",
#'     options =c("yes", "no")
#'   ),
#'   email = "johndoe@email.com"
#' ) |>
#' write_trelliscope() |>
#' view_trelliscope()
#' }
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
#' @param height Height (in lines of text) of the text box input.
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' panel_dat <- (
#'   ggplot(gapminder, aes(year, lifeExp)) +
#'     geom_point() +
#'     facet_panels()
#'   ) |>
#'   nest_panels()
#'
#' trell <- panel_dat |>
#'   as_trelliscope_df() |>
#'   write_panels() |>
#'   add_inputs(
#'     input_text(
#'       name = "Text Input",
#'       label = "A space to add custom text input")
#'     ),
#'     email = "johndoe@@email.com"
#' )
#' }
#' @family {input types}
#' @export
input_text <- function(
  name, label = name, active = TRUE, height = 3
) {
  TextInput$new(
    name = name, label = label, active = active, height = height
  ) |>
    add_input_class()
}
