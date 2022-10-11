to_json <- function(x, pretty = FALSE) {
  jsonlite::toJSON(
    x,
    pretty = pretty,
    auto_unbox = TRUE,
    digits = NA,
    null = "null"
  )
}

check_display_object <- function(obj) {
  assertthat::assert_that(inherits(obj, "trelliscope_display"),
    msg = "Expecting a trelliscope display object")
}

check_scalar <- function(x, name, err_fn = paste0) {
  assertthat::assert_that(length(x) == 1,
    msg = err_fn(paste0("'", name, "' must be a scalar value")))
}

check_integer <- function(x, name, err_fn = paste0) {
  # not strictly checking integers e.g. 1L but any rounded number
  assertthat::assert_that(is.numeric(x) && all(round(x) == x, na.rm = TRUE),
    msg = err_fn(paste0("'", name, "' must be an integer")))
}

check_enum <- function(x, vals, name, err_fn = paste0) {
  assertthat::assert_that(x %in% vals,
    msg = err_fn(paste0("'", name, "' must be one of ",
      paste(vals, collapse = ", "))))
}

check_atomic <- function(x, name, err_fn = paste0) {
  assertthat::assert_that(is.atomic(x),
    msg = err_fn(paste0(
      "'", name, "' must be an atomic vector")))
}

check_atomic_vector <- function(x, name, err_fn = paste0) {
  assertthat::assert_that(is.atomic(x) && length(x) > 0,
    msg = err_fn(paste0(
      "'", name, "' must be an atomic vector with at least one element")))
}

check_character <- function(x, name, err_fn = paste0) {
  assertthat::assert_that(is.character(x),
    msg = err_fn(paste0("'", name, "' must be of type 'character'")))
}

check_numeric <- function(x, name, err_fn = paste0) {
  assertthat::assert_that(is.numeric(x),
    msg = err_fn(paste0("'", name, "' must be numeric")))
}

check_logical <- function(x, name, err_fn = paste0) {
  assertthat::assert_that(is.logical(x),
    msg = err_fn(paste0("'", name, "' must be logical")))
}

check_exhaustive_levels <- function(x, levels, name, err_fn = paste0) {
  dff <- setdiff(
    unique(as.character(x)),
    as.character(levels)
  )
  assertthat::assert_that(length(dff) == 0,
    msg = err_fn(paste0("'", name, "' contains values not ",
      "specified in 'levels': ", paste(dff, collapse = ", "))))
}

check_date <- function(x, name, err_fn = paste0) {
  assertthat::assert_that(inherits(x, "Date"),
    msg = err_fn(paste0("'", name, "' must have class 'Date'")))
}

check_datetime <- function(x, name, err_fn = paste0) {
  assertthat::assert_that(inherits(x, "POSIXct"),
    msg = err_fn(paste0("'", name, "' must have class 'POSIXct'")))
}

check_has_var <- function(df, name, err_fn = paste0) {
  assertthat::assert_that(name %in% names(df),
    msg = err_fn(paste0("Could not find variable '", name,
      "' in the dataset that the meta definitions are being applied to")))
}

check_graphvar <- function(x, name, idvar, idvarname, err_fn = paste0) {
  assertthat::assert_that(is.list(x),
    msg = err_fn(paste0("The variable specifying the graph, '",
      name, "' must be a list")))
  ids <- unlist(unlist(x, recursive = FALSE), recursive = FALSE)
  assertthat::assert_that(is.atomic(ids),
    msg = err_fn(paste0("The values in the variable specifying the graph, '",
      name, "' must be atomic")))
  assertthat::assert_that(all(unique(ids) %in% idvar),
    msg = err_fn(paste0("There were values found in the variable specifying ",
      "the graph, '", name, "' that are not found in the ID variable, '",
      idvarname, "'")))
}

check_latvar <- function(x, name, err_fn = paste0) {
  assertthat::assert_that(is.numeric(x) && all(abs(x) <= 90, na.rm = TRUE),
    msg = err_fn(paste0("The variable specifying latitude, '",
      name, "' must be numeric and between -90 and 90")))
}

check_longvar <- function(x, name, err_fn = paste0) {
  assertthat::assert_that(is.numeric(x) &&
    all(x >= 0 & x <= 180, na.rm = TRUE),
    msg = err_fn(paste0("The variable specifying longitude, '",
      name, "' must be numeric and between 0 and 180")))
}

# sanitize <- function(x) {
#   gsub("[^a-zA-Z0-9_]", "_", x)
# }
