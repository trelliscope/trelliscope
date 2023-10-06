#' Add a "calculated panel" column to a dataset
#' @param plot_fn A function that produces a panel from a given subset of
#'   `data`, taking as arguments any variable names in `data` that you would
#'   like to be available in the plottinf function.
#' @param data A data frame from which subsets will be extracted and plots will
#'   be made. Should be a superset of the summary dataset to which this plot
#'   column is being added.
#' @export
panel_lazy <- function(
  plot_fn, data = dplyr::pick(dplyr::everything())
) {
  assert(is.function(plot_fn),
    msg = "`plot_fn` must be a function")

  vars <- names(formals(plot_fn))

  not_found <- setdiff(vars, names(data))
  assert(length(not_found) == 0,
    msg = "All function arguments of `plot_fn()` must be found in `data`.\
      Following variable{?s} not found: {not_found}")

  var_vals <- get_var_vals_list(data, vars)

  p <- do.call(plot_fn, var_vals[[1]])
  if (inherits(p, "htmlwidget")) {
    type <- "htmlwidget"
  } else if (inherits(p, "ggplot")) {
    type <- "ggplot"
  } else {
    stop("plot_fn must return either an htmlwidget or a ggplot object")
  }

  name_idx <- which(unlist(lapply(data[vars], is.atomic)))

  ids <- apply(data[vars[name_idx]], 1, function(x) paste(
    sanitize(x), sep = "_"))

  if (length(unique(ids)) != nrow(data)) {
    wrn("The atomic input columns to the lazy panel function \
      are not unique.")
  }

  vctrs::new_rcrd(
    fields = list(vars = var_vals),
    plot_fn = plot_fn,
    vars = vars,
    name_idx = name_idx,
    data = data,
    type = type,
    class = "panel_lazy_vec"
  )
}

get_var_vals_list <- function(dat, vars, top = FALSE) {
  if (top)
    dat <- head(dat, 1)

  # get vars values for each row
  by_vals <- dat |>
    dplyr::select(dplyr::all_of(vars)) |>
    split(seq_len(nrow(dat))) |>
    unname() |>
    lapply(function(x) {
      x <- as.list(x)
      lapply(x, function(a) {
        if (is.factor(a))
          a <- as.character(a)
        a
      })
    })

  by_vals
}

get_panel_rel_path <- function(x, name, fmt = NULL) {
  UseMethod("get_panel_rel_path")
}

#' @export
get_panel_rel_path.panel_lazy_vec <- function(x, name, fmt) {
  idx <- attr(x, "name_idx")
  tmp <- unlist(lapply(vec_data(x)$vars, function(a)
    paste(sanitize(a[idx]), collapse = "_")))
  file.path("panels", sanitize(name), paste0(tmp, ".", fmt))
}

get_panel <- function(x) {
  UseMethod("get_panel")
}

# only meant to work if x is a single element
#' @export
get_panel.panel_lazy_vec <- function(x) {
  vars <- attr(x, "vars")
  plot_fn <- attr(x, "plot_fn")
  var_vals <- vctrs::vec_data(x)$vars[[1]]
  do.call(plot_fn, var_vals)
}

#' @export
format.panel_lazy_vec <- function(x, ...) {
  # vctrs::field(x, "path")
  if (length(x) == 1)
    print(get_panel(x))
  rep(paste0("<", attr(x, "type"), ">"), length(x))
}

#' @importFrom vctrs vec_ptype_abbr
#' @export
vec_ptype_abbr.panel_lazy_vec <- function(
  x, ..., prefix_named = FALSE, suffix_shape = TRUE
) {
  "lazy_panels"
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.panel_lazy_vec <- function(x, ...) {
  out <- rep(paste0("<", attr(x, "type"), ">"), length(x))
  pillar::new_pillar_shaft_simple(out, align = "left")
}

# #' @export
# obj_print_data.panel_lazy_vec <- function(x) {
#   cat(format(x), sep = "\n")
# }

get_panel_type <- function(x, type) {
  if (is.character(type) && length(type) == 1 && type %in% c("img", "frame"))
    return(type)
  type <- "iframe"
  exts <- tolower(unique(tools::file_ext(x)))
  if (all(exts %in% valid_img_exts)) {
    type <- "img"
  }
  type
}

#' Add a "panel_url" column to a dataset
#' @param urls A character vector of URLs to be used as panels.
#' @param type The "type" of panel ("img" or "iframe"). If NULL, will be
#'  inferred from the file extension.
#' @importFrom vctrs field new_rcrd new_vctr vec_data
#' @export
panel_url <- function(urls, type = NULL) {
  assert(is.character(urls),
    msg = "`urls` must be a character vector")

  vctrs::new_vctr(
    urls,
    type = get_panel_type(urls, type),
    class = "panel_url_vec"
  )
}

#' @export
get_panel_rel_path.panel_url_vec <- function(x, name, fmt = NULL) {
  x
}

#' @export
vec_ptype.panel_url_vec.panel_url_vec <- function(x, y, ...) panel_url()
#' @export
vec_ptype.panel_url_vec.character <- function(x, y, ...) panel_url()
#' @export
vec_ptype.character.panel_url_vec <- function(x, y, ...) panel_url()

#' @export
vec_cast.panel_url_vec.panel_url_vec <- function(x, to, ...) x
#' @export
vec_cast.panel_url_vec.character <- function(x, to, ...) panel_url(x)
#' @export
vec_cast.character.panel_url_vec <- function(x, to, ...) vctrs::vec_data(x)

# only meant to work if x is a single element
#' @export
get_panel.panel_url_vec <- function(x) {
  utils::browseURL(vctrs::vec_data(x))
}

#' @export
format.panel_url_vec <- function(x, ...) {
  if (length(x) == 1)
    print(get_panel(x))
  vctrs::vec_data(x)
}

#' @importFrom vctrs vec_ptype_abbr
#' @export
vec_ptype_abbr.panel_url_vec <- function(
  x, ..., prefix_named = FALSE, suffix_shape = TRUE
) {
  "url_panels"
}

#' @importFrom pillar pillar_shaft style_subtle
#' @export
pillar_shaft.panel_url_vec <- function(x, ...) {
  out <- vctrs::vec_data(x)
  nc <- nchar(out)
  out_short <- paste0("<", attr(x, "type"), ">")
  out <- paste0(out_short,
    pillar::style_subtle(paste0(" ...", substr(out, nc - 10, nc))))
  pillar::new_pillar_shaft_simple(out, align = "left",
    short_formatted = rep(out_short, length(x)))
}

#' Add a "panel_local" column to a dataset
#' @param x A character vector of paths to local files to be used as panels.
#' @param type The "type" of panel ("img" or "iframe"). If NULL, will be
#'  inferred from the file extension.
#' @export
panel_local <- function(x = character(), type = NULL) {
  assert(is.character(x),
    msg = "`x` must be a character vector")

  vctrs::new_vctr(
    x,
    type = get_panel_type(x, type),
    class = "panel_local_vec"
  )
}

#' @export
get_panel_rel_path.panel_local_vec <- function(x, name, fmt = NULL) {
  file.path("panels", sanitize(name), basename(x))
}

#' @export
vec_ptype.panel_local_vec.panel_local_vec <- function(x, y, ...) panel_local()
#' @export
vec_ptype.panel_local_vec.character <- function(x, y, ...) panel_local()
#' @export
vec_ptype.character.panel_local_vec <- function(x, y, ...) panel_local()

#' @export
vec_cast.panel_local_vec.panel_local_vec <- function(x, to, ...) x
#' @export
vec_cast.panel_local_vec.character <- function(x, to, ...) panel_local(x)
#' @export
vec_cast.character.panel_local_vec <- function(x, to, ...) vctrs::vec_data(x)

# only meant to work if x is a single element
#' @importFrom magick image_ggplot image_read
#' @export
get_panel.panel_local_vec <- function(x) {
  if (attr(x, "type") == "iframe") {
    utils::browseURL(vctrs::vec_data(x))
  } else {
    magick::image_ggplot(magick::image_read(vctrs::vec_data(x)))
  }
}

#' @export
format.panel_local_vec <- function(x, ...) {
  if (length(x) == 1)
    print(get_panel(x), info = FALSE)
  vctrs::vec_data(x)
}

#' @importFrom vctrs vec_ptype_abbr
#' @export
vec_ptype_abbr.panel_local_vec <- function(
  x, ..., prefix_named = FALSE, suffix_shape = TRUE
) {
  "local_panels"
}

#' @importFrom pillar pillar_shaft new_pillar_shaft_simple style_subtle
#' @export
pillar_shaft.panel_local_vec <- function(x, ...) {
  out <- vctrs::vec_data(x)
  nc <- nchar(out)
  out_short <- paste0("<", attr(x, "type"), ">")
  out <- paste0(out_short,
    pillar::style_subtle(paste0(" ...", substr(out, nc - 10, nc))))
  pillar::new_pillar_shaft_simple(out, align = "left",
    short_formatted = rep(out_short, length(out)))
}
