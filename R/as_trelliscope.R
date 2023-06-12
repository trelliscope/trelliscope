#' Instantiate a trelliscope data frame
#' @param df A data frame that contains the metadata of the display as well as
#' a column that indicate the panels to be displayed.
#' @param name Name of the trelliscope display.
#' @param description Description of the trelliscope display.
#' @param tags Optional vector of tag names to identify the display in the
#'   case that there are many to search through.
#' @param key_cols Variable names in `df` that uniquely define a row of the
#'   data. If not supplied, an attempt will be made to infer them.
#' @param path Directory in which to place the trelliscope display when
#'   it is written using [`write_trelliscope()`].
#' @param force_plot Should the panels be forced to be plotted, even if they
#'   have already been plotted and have not changed since the previous plotting?
#' @param key_sig A string "signature" that represents the panels for this
#'   display. This should not be specified unless you know what you are doing.
#'
#' @examples
#' # Use `as_trelliscope_df()` to convert panel metadata to a special
#' # trelliscope data frame
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' panel_dat <- (
#'   ggplot(gapminder, aes(year, lifeExp)) +
#'     geom_point() +
#'     facet_panels(~country + continent)
#'   ) |>
#'     nest_panels()
#'
#' meta_dat <- gapminder |>
#'   group_by(country, continent) |>
#'   summarise(
#'     mean_lifeExp = mean(lifeExp),
#'     min_lifeExp = min(lifeExp),
#'     max_lifeexp = max(lifeExp),
#'     mean_gdp = mean(gdpPercap),
#'     .groups = "drop"
#'   )
#'
#' joined_dat <- left_join(panel_dat, meta_dat) |>
#'   as_trelliscope_df(name = "life_expectancy", path = tempfile())
#'
#' disp <- joined_dat |>
#'   write_panels() |>
#'   write_trelliscope() |>
#'   view_trelliscope()
#' }
#'
#' # You can also use `as_trelliscope_df()` on datasets that have links to
#' # images instead of conventional ggplot objects
#' \dontrun{
#' }
#' @export
#' @importFrom utils head
#' @importFrom dplyr group_cols
as_trelliscope_df <- function(
  df, name = NULL, description = name, key_cols = NULL, tags = NULL,
  path = NULL, force_plot = FALSE, key_sig = NULL
) {
  if (inherits(df, "facet_panels")) {
    # msg("
    #   An object from {.fn facet_panels} was passed to {.fn trelliscope}.
    #   {.emph Building panels...}")
    msg("{.emph Note:} For more control over building panels, you can \\
      call {.fn as_panels_df} explicitly before passing to {.fn trelliscope}.",
      .frequency = "regularly", .frequency_id = "explicit_build_note")
    df <- as_panels_df(df)
  }

  if (is.null(key_cols))
    key_cols <- get_keycols(df)

  if (is.null(name)) {
    name <- attr(df, "trelliscope")$name
    if (is.null(name)) {
      wrn("A name for the display was not specified. Provide a {.field name} \
        when calling {.fn as_trelliscope_df}")
      name <- "Trelliscope"
    }
  }

  if (in_rmarkdown()) {
    if (is.null(path)) {
      path <- file.path(knitr::opts_chunk$get("fig.path"),
        knitr::opts_current$get("label"))
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
    } else {
      assert(dirname(path) == ".",
        msg = "When using Trelliscope in RMarkdown, \
          output path must be relative")
    }
    # if (!can_print_rmarkdown())
    #   wrn("Trelliscope shoud only be rendered in RMarkdown when \
    #     self_contained is false. Trelliscope always produces auxiliary files")
  } else if (!is.null(shiny::getCurrentOutputInfo()$name)) {
    has_rsrc_path <- FALSE
    if (!is.null(path))
      has_rsrc_path <- normalizePath(dirname(path)) %in% shiny::resourcePaths()
    if (!is.null(path)) {
      if (!has_rsrc_path) {
        msg("Overwriting path for trelliscope display because it is being \
          built from within a Shiny app and the specified path is not found \
          in shiny::resourcePaths().")
        path <- file.path("www/trelliscope", shiny::getCurrentOutputInfo()$name)
      }
    } else {
      path <- file.path("www/trelliscope", shiny::getCurrentOutputInfo()$name)
    }
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  } else if (is.null(path)) {
    path <- tempfile()
  }

  obj <- Display$new(name = name, description = description,
    keycols = key_cols, path = path, force_plot = force_plot,
    tags = tags)
  class(obj) <- c("R6", "trelliscope_object")

  attr(df, "trelliscope") <- obj
  if (!inherits(df, "trelliscope"))
    class(df) <- c("trelliscope", class(df))

  df <- find_panel_vars(df)

  df
}

excl_types <- c("ggpanel_vec", "panel_lazy_vec", "panel_local_vec",
  "panel_url_vec", "href_vec")

find_panel_vars <- function(trdf, warn = TRUE) {
  not_panels <- unlist(lapply(trdf, function(x) !inherits(x, excl_types)))
  nms <- names(trdf)[not_panels]

  for (nm in nms) {
    x <- trdf[[nm]]
    if (is.character(x)) {
      exts <- tolower(unique(tools::file_ext(x)))
      http_pref <- all(grepl("^http:", x))
      all_imgs <- all(exts %in% valid_img_exts)
      all_html <- all(exts %in% c("html", "htm"))
      # ?: should all URLs be coerced to be panels?
      # (currently only coerce if there is an image extension)
      if (http_pref && all_imgs) {
        trdf[[nm]] <- panel_url(x)
      } else if (!http_pref && (all_imgs || all_html)) {
        trdf[[nm]] <- panel_local(x)
      }
    }
  }

  if (warn) {
    # now warn if local panels are not in the right place for trelliscope
    local_panels <- unlist(lapply(trdf, function(x)
      inherits(x, "panel_local_vec")))
    tr_path <- attr(trdf, "trelliscope")$get_display_path()

    for (nm in names(trdf)[local_panels]) {
      x <- trdf[[nm]]
      panel_path <- file.path(tr_path, "panels", sanitize(nm))
      if (!dir.exists(panel_path))
        dir.create(panel_path, recursive = TRUE)
      panel_path <- tools::file_path_as_absolute(panel_path)
      x2 <- x[file.exists(x)]
      if (length(x2) == 0) {
        wrn("No files for local panel {.val {nm}} were found.")
        next
      }
      udir <- unique(dirname(x2))
      udir <- udir[dir.exists(udir)]
      idir <- tools::file_path_as_absolute(udir[1])
      if (idir != panel_path)
        wrn("Files for local panel {.val {nm}} are not in the correct \\
          location. They are currently here: '{idir}' and will be copied \\
          here: '{panel_path}' when the display is written.")
    }
  }

  trdf
}

get_keycols <- function(df) {
  if (!is.null(attr(df, "trelliscope")$facet_cols)) {
    keycols <- attr(df, "trelliscope")$facet_cols
  } else if (!is.null(attr(df, "keycols"))) {
    keycols <- attr(df, "keycols")
  } else if (inherits(df, "grouped_df")) {
    idx <- dplyr::group_cols(data = df)
    keycols <- names(df)[idx]
    df <- dplyr::ungroup(df)
  } else {
    n <- nrow(df)
    nms <- names(df)
    # use character columns first to find unique, then numeric
    char_cols <- which(unname(unlist(lapply(df, function(x)
      inherits(x, c("character", "factor", "integer", "Date", "POSIXct")) &&
      !inherits(x, "img_panel")))))
    num_cols <- which(unname(unlist(lapply(df, function(x)
      is.numeric(x)))))
    num_cols <- setdiff(num_cols, char_cols)
    keycols <- character(0)
    all_cols <- c(char_cols, num_cols)
    for (ii in seq_along(all_cols)) {
      if (nrow(dplyr::distinct(df[utils::head(all_cols, ii)])) == n) {
        keycols <- nms[utils::head(all_cols, ii)]
        break
      }
    }
    assert(length(keycols) > 0,
      "Could not find columns of the data that uniquely define each row.")
  }

  if (is.null(attr(df, "trelliscope")$facet_cols))
    msg("Using the variable{?s} {.val {keycols}} \\
      to uniquely identify each row of the data.")

  keycols
}
