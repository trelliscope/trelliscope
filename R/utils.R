to_json <- function(x, pretty = FALSE, factor = "string", force = FALSE) {
  jsonlite::toJSON(
    x,
    pretty = pretty,
    auto_unbox = TRUE,
    digits = NA,
    null = "null",
    POSIXt = "ISO8601",
    factor = factor,
    force = force
  )
}

check_trelliscope_df <- function(obj) {
  # assert(inherits(attr(obj, "trelliscope"), "trelliscope_object"),
  #   msg = "Expecting a trelliscope data frame")
  assert(inherits(obj, "data.frame"),
    msg = "Expecting a trelliscope data frame or data.frame")

  if (!inherits(obj, "trelliscope"))
    obj <- as_trelliscope_df(obj)

  obj
}

# check_trelliscope_object <- function(obj) {
#   assert(inherits(obj, "trelliscope_object"),
#     msg = "Expecting a trelliscope object")
# }

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

check_not_has_var <- function(df, name, err_fn = paste0) {
  assert(!name %in% names(df),
    msg = err_fn("Variable {.val {name}} already exists in the data"))
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
    all(abs(x) <= 180, na.rm = TRUE),
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

empty_object <- function() {
  structure(list(), names = character(0))
}

compute_digits <- function(x) {
  nu <- length(unique(x))
  digits <- -1
  for (ii in 0:5) {
    if (length(unique(round(x, ii))) / nu > 0.75) {
      digits <- ii
      break
    }
  }
  if (digits < 0)
    digits <- 0
  digits
}

valid_img_exts <- c("apng", "avif", "gif", "jpg", "jpeg", "jfif", "pjpeg",
  "pjp", "png", "svg", "webp")

find_img_col <- function(df) {
  res <- character(0)
  chr_cols <- lapply(df, is.character) |>
    unlist() |>
    which() |>
    names()
  for (nm in chr_cols) {
    cur_ext <- tolower(tools::file_ext(df[[nm]][1]))
    if (cur_ext %in% valid_img_exts) {
      all_exts <- tools::file_ext(df[[nm]])
      if (length(unique(all_exts)) == 1) {
        res <- nm
        msg("Using {.field {nm}} as image column")
        break
      }
    }
  }
  res
}

# set.seed(1234)
# aa <- rnorm(100)
# compute_digits(aa)
# aa <- rnorm(100, sd = 10)
# compute_digits(aa)
# aa <- rnorm(100, sd = 100)
# compute_digits(aa)

# basic idea: fit line to normal quantile plot of raw and log10
# and compare R-squared values
#' @importFrom stats na.omit qnorm ppoints lm
# @importFrom robustbase ltsReg
needs_log <- function(x) {
  x <- stats::na.omit(x)
  if (any(x <= 0) || length(x) == 0)
    return(FALSE)

  tryres <- try({
    testdat1 <- data.frame(x = sort(x), y = stats::qnorm(
      stats::ppoints(length(x))))
    # p1 <- summary(
    #   robustbase::ltsReg(y ~ x, alpha = 0.95, data = testdat1))$r.squared
    p11 <- summary(stats::lm(y ~ x, data = testdat1))$r.squared

    testdat2 <- data.frame(x = sort(log10(x)), y = stats::qnorm(
      stats::ppoints(length(x))))
    # p2 <- summary(
    #   robustbase::ltsReg(y ~ x, alpha = 0.95, data = testdat2))$r.squared
    p22 <- summary(stats::lm(y ~ x, data = testdat2))$r.squared
  }, silent = TRUE)
  if (inherits(tryres, "try-error"))
    return(FALSE)

  p22 > (p11 + 0.2)
}

# could use rlang::hash but it is more persnickety
#' @importFrom digest digest
hash <- function(x)
  digest::digest(x)

# sapply(1:100, function(a) {
#   x <- abs(rcauchy(1000))
#   needs_log(x)
# }) |> table()

# sapply(1:100, function(a) {
#   x <- abs(rnorm(1000))
#   needs_log(x)
# }) |> table()

# sapply(1:100, function(a) {
#   x <- rexp(1000)
#   needs_log(x)
# }) |> table()

# sapply(1:100, function(a) {
#   x <- runif(1000)
#   needs_log(x)
# }) |> table()

# sapply(1:100, function(a) {
#   x <- rweibull(1000, 0.5)
#   needs_log(x)
# }) |> table()

# sapply(1:100, function(a) {
#   x <- rweibull(1000, 1)
#   needs_log(x)
# }) |> table()

# sapply(1:100, function(a) {
#   x <- rweibull(1000, 0.75)
#   needs_log(x)
# }) |> table()

# sapply(1:100, function(a) {
#   x <- rweibull(1000, 0.6)
#   needs_log(x)
# }) |> table()
