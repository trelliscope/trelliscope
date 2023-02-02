to_json <- function(x, pretty = FALSE) {
  jsonlite::toJSON(
    x,
    pretty = pretty,
    auto_unbox = TRUE,
    digits = NA,
    null = "null",
    POSIXt = "ISO8601"
  )
}

check_display_object <- function(obj) {
  assert(inherits(obj, "trelliscope_display"),
    msg = "Expecting a trelliscope display object")
}

check_state_object <- function(obj) {
  assert(inherits(obj, "trelliscope_state_def"),
    msg = "Expecting a trelliscope state definition object")
}

check_scalar <- function(x, name, err_fn = paste0) {
  assert(length(x) == 1,
    msg = err_fn("{.val {name}} must be a scalar value"))
}

check_integer <- function(x, name, err_fn = paste0) {
  # not strictly checking integers e.g. 1L but any rounded number
  assert(is.numeric(x) && all(round(x) == x, na.rm = TRUE),
    msg = err_fn("{.val {name}} must be an integer"))
}

check_enum <- function(x, vals, name, err_fn = paste0) {
  assert(all(x %in% vals),
    msg = err_fn("{.val {name}} must be one of {.val {vals}}"))
}

check_atomic <- function(x, name, err_fn = paste0) {
  assert(is.atomic(x),
    msg = err_fn("{.val {name}} must be an atomic vector"))
}

check_atomic_vector <- function(x, name, err_fn = paste0) {
  assert(is.atomic(x) && length(x) > 0,
    msg = err_fn("{.val {name}} must be an atomic vector with at least \\
      one element"))
}

check_character <- function(x, name, err_fn = paste0) {
  assert(is.character(x),
    msg = err_fn("{.val {name}} must be of type 'character'"))
}

check_numeric <- function(x, name, err_fn = paste0) {
  assert(is.numeric(x),
    msg = err_fn("{.val {name}} must be numeric"))
}

check_pos_numeric <- function(x, name, err_fn = paste0) {
  assert(is.numeric(x) && x > 0,
    msg = err_fn("{.val {name}} must be numeric and positive"))
}

check_logical <- function(x, name, err_fn = paste0) {
  assert(is.logical(x),
    msg = err_fn("{.val {name}} must be logical"))
}

check_exhaustive_levels <- function(x, levels, name, err_fn = paste0) {
  dff <- setdiff(
    unique(as.character(x)),
    as.character(levels)
  )
  assert(length(dff) == 0,
    msg = err_fn("{.val {name}} contains values not \\
      specified in 'levels': {.val {dff}}"))
}

check_date <- function(x, name, err_fn = paste0) {
  assert(inherits(x, "Date"),
    msg = err_fn("{.val {name}} must have class 'Date'"))
}

check_datetime <- function(x, name, err_fn = paste0) {
  assert(inherits(x, "POSIXct"),
    msg = err_fn("{.val {name}} must have class 'POSIXct'"))
}

check_has_var <- function(df, name, err_fn = paste0) {
  assert(name %in% names(df),
    msg = err_fn("Could not find variable {.val {name}} \\
      in the dataset that the meta definitions are being applied to"))
}

check_graphvar <- function(
  x, name, idvar, idvarname, err_fn = paste0
) {
  assert(is.list(x),
    msg = err_fn("The variable specifying the graph, \\
      {.val {name}} must be a list"))
  ids <- unlist(unlist(x, recursive = FALSE), recursive = FALSE)
  assert(is.atomic(ids),
    msg = err_fn("The values in the variable specifying the graph \\
      {.val {name}} must be atomic"))
  assert(all(unique(ids) %in% idvar),
    msg = err_fn("There were values found in the variable specifying \\
      the graph {.val {name}} that are not found in the ID variable, \\
      {.val {idvarname}}"))
}

check_latvar <- function(x, name, err_fn = paste0) {
  assert(is.numeric(x) && all(abs(x) <= 90, na.rm = TRUE),
    msg = err_fn("The variable specifying latitude, \\
      {.val {name}} must be numeric and between -90 and 90"))
}

check_longvar <- function(x, name, err_fn = paste0) {
  assert(is.numeric(x) &&
    all(x >= 0 & x <= 180, na.rm = TRUE),
    msg = err_fn("The variable specifying longitude, \\
      {.val {name}} must be numeric and between 0 and 180"))
}

sanitize <- function(x) {
  gsub("[^a-zA-Z0-9_]", "_", x)
}

#' @importFrom cli cli_abort
assert <- function(cond, msg, envir = parent.frame()) {
  if (!cond)
    cli::cli_abort(msg, .envir = envir)
  TRUE
}

#' @importFrom cli cli_inform
msg <- function(x, envir = parent.frame(), ...) {
  cli::cli_inform(c("i" = x), .envir = envir, ...)
}

#' @importFrom cli cli_warn
wrn <- function(x, envir = parent.frame(), ...) {
  cli::cli_warn(c("!" = x), .envir = envir, ...)
}
