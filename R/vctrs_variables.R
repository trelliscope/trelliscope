#' Create a number vector
#' @param x A numeric vector.
#' @param digits How many digits to round to when displaying the number.
#' If not specified, a value will be inferred. If -1, all digits will be shown.
#' @param locale Should the variable be displayed using its locale?
#' For example, 1234.56 in US would be displayed as 1,234.56.
#' @param log Should the variable's distribution be shown on the log scale?
#' If not specified, an inference will be made based on its values.
#' @import vctrs
#' @export
number <- function(x = double(),
  digits = 2,
  locale = TRUE,
  log = NULL
) {
  assert(is.numeric(x),
    msg = "`x` must be a numeric vector")

  if (!is.null(digits))
    assert(is.numeric(digits) && length(digits) == 1,
      msg = "`digits` must be a single numeric value")

  assert(is.logical(locale) && length(locale) == 1,
    msg = "`locale` must be a single logical value")

  if (!is.null(log))
    assert(is.numeric(log) && length(log) == 1,
      msg = "`log` must be a single logical value")

  vctrs::new_vctr(
    x,
    digits = digits,
    locale = locale,
    log = log,
    class = "number_vec"
  )
}

#' @export
vec_ptype2.number_vec.number_vec <- function(x, y, ...) number()
#' @export
vec_ptype2.number_vec.double <- function(x, y, ...) number()
#' @export
vec_ptype2.double.number_vec <- function(x, y, ...) number()

# TODO: make sure existing attributes are captured
#' @export
vec_cast.number_vec.number_vec <- function(x, to, ...) x
#' @export
vec_cast.number_vec.double <- function(x, to, ...) number(x)
#' @export
vec_cast.double.number_vec <- function(x, to, ...) vctrs::vec_data(x)

#' @export
format.number_vec <- function(x, ...) {
  digits <- attr(x, "digits")
  out <- vctrs::vec_data(x)
  if (!is.null(digits))
    out <- sprintf(paste0("%.", digits, "f"), out)
  # TODO: locale and log
  out
}

#' @importFrom vctrs vec_ptype_abbr
#' @export
vec_ptype_abbr.number_vec <- function(
  x, ..., prefix_named = FALSE, suffix_shape = TRUE
) {
  "number"
}

#' Create a currency vector
#' @param x A numeric vector.
#' @param code Currency code. See [currencies] for a list of possibilities.
#' @param digits How many digits to round to when displaying the number.
#' If `NULL`, all digits will be shown. Default is 2.
#' @param locale Should the variable be displayed using its locale?
#' @param log Should the variable's distribution be shown on the log scale?
#' If not specified, an inference will be made based on its values.
currency <- function(x = double(),
  code = "USD",
  digits = 2,
  locale = TRUE,
  log = NULL
) {
  assert(is.numeric(x),
    msg = "`x` must be a numeric vector")

  assert(code %in% trelliscope::currencies$code_alpha,
    msg = "`code` must be a valid currency code")

  if (!is.null(digits))
    assert(is.numeric(digits) && length(digits) == 1,
      msg = "`digits` must be a single numeric value")

  assert(is.logical(locale) && length(locale) == 1,
    msg = "`locale` must be a single logical value")

  if (!is.null(log))
    assert(is.numeric(log) && length(log) == 1,
      msg = "`log` must be a single logical value")

  vctrs::new_vctr(
    x,
    code = code,
    digits = digits,
    locale = locale,
    log = log,
    class = "currency_vec"
  )
}

#' @export
vec_ptype2.currency_vec.currency_vec <- function(x, y, ...) currency()
#' @export
vec_ptype2.currency_vec.double <- function(x, y, ...) currency()
#' @export
vec_ptype2.double.currency_vec <- function(x, y, ...) currency()

# TODO: make sure existing attributes are captured
#' @export
vec_cast.currency_vec.currency_vec <- function(x, to, ...) x
#' @export
vec_cast.currency_vec.double <- function(x, to, ...) currency(x)
#' @export
vec_cast.double.currency_vec <- function(x, to, ...) vctrs::vec_data(x)

#' @export
format.currency_vec <- function(x, ...) {
  digits <- attr(x, "digits")
  out <- vctrs::vec_data(x)
  if (!is.null(digits))
    out <- sprintf(paste0("%.", digits, "f"), out)
  # TODO: symbol, locale, and log
  out
}

#' @importFrom vctrs vec_ptype_abbr
#' @export
vec_ptype_abbr.currency_vec <- function(
  x, ..., prefix_named = FALSE, suffix_shape = TRUE
) {
  "currency"
}

#' Create a vector of links
#' @param x A character vector.
#' @export
href <- function(x = character()) {
  assert(is.character(x),
    msg = "`x` must be a character vector")

  vctrs::new_vctr(x, class = "href_vec")
}

#' @export
vec_ptype2.href_vec.href_vec <- function(x, y, ...) href()
#' @export
vec_ptype2.href_vec.double <- function(x, y, ...) href()
#' @export
vec_ptype2.double.href_vec <- function(x, y, ...) href()

# TODO: make sure existing attributes are captured
#' @export
vec_cast.href_vec.href_vec <- function(x, to, ...) x
#' @export
vec_cast.href_vec.double <- function(x, to, ...) href(x)
#' @export
vec_cast.double.href_vec <- function(x, to, ...) vctrs::vec_data(x)

#' @importFrom vctrs vec_ptype_abbr
#' @export
vec_ptype_abbr.href_vec <- function(
  x, ..., prefix_named = FALSE, suffix_shape = TRUE
) {
  "href"
}
