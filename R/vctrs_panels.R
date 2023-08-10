#' Add a "calculated panel" column to a dataset
#' @param plot_fn A function that produces a panel from a given subset of
#'   `data`.
#' @param data A data frame from which subsets will be extracted and plots will
#'   be made. Should be a superset of the summary dataset to which this plot
#'   column is being added.
#' @param by A list of variables found in both `data` and in the summary
#'   dataset to which this plot column is being added. This is used to specify
#'   which subset of `data` to apply for a given plot. If not provided, it is
#'   inferred based on the variables found in both `data` and the summary data.
#' @param cur A data frame containing the current summary dataset to which this
#'   plot column is being added. This is used to determine which variables in
#'   `data` are available for subsetting. Ideally you should not change this
#'   parameter.
#' @export
panel_lazy <- function(
  plot_fn, data, by = NULL, cur = dplyr::pick(dplyr::everything())
) {
  assert(is.function(plot_fn),
    msg = "`plot_fn` must be a function")
  by_vals <- get_by_vals(data, cur, by = by)
  by <- names(by_vals[[1]])

  # test plot function on a subset
  nd <- data
  for (gv in by)
    nd <- dplyr::filter(nd, .data[[gv]] == by_vals[[1]][[gv]][[1]])
  # nd <- dplyr::collect(nd)
  p <- plot_fn(nd)
  if (inherits(p, "htmlwidget")) {
    type <- "htmlwidget"
  } else if (inherits(p, "ggplot")) {
    type <- "ggplot"
  } else {
    stop("plot_fn must return either an htmlwidget or a ggplot object")
  }

  vctrs::new_rcrd(
    fields = list(by = by_vals),
    plot_fn = plot_fn,
    by = by,
    data = data,
    type = type,
    class = "panel_lazy_vec"
  )
}

#' Get a subset of a dataset to test a plot function on
#' @param full_dat The full dataset from which to extract a subset
#' @param summ_dat The summary dataset to which a plot column will be added.
#'   Each row of this dataset will correspond to one subset of `full_dat` and a
#'   sample row will be used to produce a single test subset.
#' @param by A list of variables found in both `full_dat` and in `summ_dat`
#'   to which a plot column will be added. This is used to specify which subset
#'   of `data` to apply for a given plot. If not provided, it is inferred based
#'   on the variables found in both `data` and the summary data.
#' @export
get_test_subset <- function(full_dat, summ_dat, by = NULL) {
  by_vals <- get_by_vals(full_dat, summ_dat, by = by, top = TRUE)
  by <- names(by_vals[[1]])
  nd <- full_dat
  for (gv in by)
    nd <- dplyr::filter(nd, .data[[gv]] == by_vals[[1]][[gv]][[1]])
  nd
}

get_by_vals <- function(full_dat, summ_dat, by = NULL, top = FALSE) {
  is_df <-
    (inherits(full_dat, "Dataset") && inherits(full_dat, "ArrowObject")) ||
    inherits(full_dat, "arrow_dplyr_query") ||
    is.data.frame(full_dat)

  assert(is_df,
    msg = "`full_dat` must be a data frame or appropriate Arrow object")
  if (!is.null(by)) {
    assert(is.character(by),
      msg = "`by` must be a character vector")
    assert(all(by %in% names(full_dat)),
      msg = "All elements of `by` must be found in `full_dat`")
  }

  if (is.null(by)) {
    by <- intersect(names(full_dat), names(summ_dat))
    # TODO: should make sure these are atomic, etc.
  }

  if (top)
    summ_dat <- head(summ_dat, 1)

  # get "by" values for each row
  by_vals <- summ_dat |>
    dplyr::select(dplyr::all_of(by)) |>
    split(seq_len(nrow(summ_dat))) |>
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
  tmp <- unlist(lapply(vec_data(x)$by, function(x)
    paste(sanitize(x), collapse = "_")))
  file.path("panels", sanitize(name), paste0(tmp, ".", fmt))
}

get_panel <- function(x) {
  UseMethod("get_panel")
}

# only meant to work if x is a single element
#' @export
get_panel.panel_lazy_vec <- function(x) {
  nd <- attr(x, "data")
  by <- attr(x, "by")
  plot_fn <- attr(x, "plot_fn")
  by_vals <- vctrs::vec_data(x)$by[[1]]
  for (gv in by)
    nd <- dplyr::filter(nd, .data[[gv]] == by_vals[[gv]][[1]])
  nd <- dplyr::collect(nd)
  plot_fn(nd)
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
vec_ptype2.panel_url_vec.panel_url_vec <- function(x, y, ...) panel_url()
#' @export
vec_ptype2.panel_url_vec.character <- function(x, y, ...) panel_url()
#' @export
vec_ptype2.character.panel_url_vec <- function(x, y, ...) panel_url()

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
vec_ptype2.panel_local_vec.panel_local_vec <- function(x, y, ...) panel_local()
#' @export
vec_ptype2.panel_local_vec.character <- function(x, y, ...) panel_local()
#' @export
vec_ptype2.character.panel_local_vec <- function(x, y, ...) panel_local()

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
