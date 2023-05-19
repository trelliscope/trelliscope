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
#' @param server An experimental feature that allows your local R session to
#'   act as a server so that panels do not need to be pre-rendered. See
#'   [`local_websocket_server()`].
#' @export
#' @importFrom utils head
#' @importFrom dplyr group_cols
as_trelliscope_df <- function(
  df, name = NULL, description = name, key_cols = NULL, tags = NULL,
  path = NULL, force_plot = FALSE, key_sig = NULL, server = NULL
) {
  if (inherits(df, "facet_panels")) {
    # msg("
    #   An object from {.fn facet_panels} was passed to {.fn trelliscope}.
    #   {.emph Building panels...}")
    msg("{.emph Note:} For more control over building panels, you can \\
      call {.fn nest_panels} explicitly before passing to {.fn trelliscope}.",
      .frequency = "regularly", .frequency_id = "explicit_build_note")
    df <- nest_panels(df)
  }

  if (is.null(server)) {
    panel_col <- check_and_get_panel_col(df)
    if (length(panel_col) == 0) {
      panel_col <- find_img_col(df)
      if (length(panel_col) == 1) {
        is_remote <- all(grepl("^http", df[[panel_col]]))
        if (is_remote) {
          df[[panel_col]] <- img_panel(df[[panel_col]])
        } else {
          df[[panel_col]] <- img_panel_local(df[[panel_col]])
        }
      }
    }
    assert(length(panel_col) == 1,
      msg = paste0("Couldn't find a column in the trelliscope input ",
        "data frame that references a plot or image."))
  } else {
    df[["__server__"]] <- as.integer(NA)
    panel_col <- "__server__"
    # TODO: check server object
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
        browser()
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
    panel_col = panel_col, tags = tags, keysig = key_sig, server = server)
  class(obj) <- c("R6", "trelliscope_object")

  attr(df, "trelliscope") <- obj
  if (!inherits(df, "trelliscope"))
    class(df) <- c("trelliscope", class(df))

  df <- infer_panel_type(df)

  df
}

check_and_get_panel_col <- function(df) {
  # look for a column with one of the following classes:
  # - img_panel (which includes img_panel_local)
  # - nested_panels
  panel_col_idx <- which(unlist(lapply(df, function(a)
    inherits(a, c("img_panel", "iframe_panel", "nested_panels")))))
  if (length(panel_col_idx) > 1) {
    msg("Found multiple columns that indicate a panel, using the first \\
      one found: '{names(panel_col_idx)[1]}")
    panel_col_idx <- panel_col_idx[1]
  }
  names(panel_col_idx)
}

infer_panel_type <- function(trdf) {
  trobj <- attr(trdf, "trelliscope")$clone()
  pnls <- trdf[[trobj$panel_col]]
  if (trobj$panel_col == "__server__") {
    trobj$set("paneltype",
      ifelse(tolower(trobj$server$format) == "html", "iframe", "img"))
  } else if (inherits(pnls, "nested_panels")) {
    panel1 <- pnls[[1]]
    if (inherits(panel1, "htmlwidget")) {
      trobj$set("paneltype", "iframe")
    } else  {
      trobj$set("paneltype", "img")
    }
  } else if (inherits(pnls, "img_panel")) {
      trobj$set("paneltype", "img")
      trobj$set("panelaspect", attr(pnls, "aspect_ratio"))
      trobj$panels_written <- NA
      trdf <- dplyr::rename(trdf, "__PANEL_KEY__" := trobj$panel_col)
      trobj$panel_col <- "__PANEL_KEY__"
  } else if (inherits(pnls, "iframe_panel")) {
      trobj$set("paneltype", "iframe")
      trobj$set("panelaspect", attr(pnls, "aspect_ratio"))
      trobj$panels_written <- NA
      trdf <- dplyr::rename(trdf, "__PANEL_KEY__" := trobj$panel_col)
      trobj$panel_col <- "__PANEL_KEY__"
  } else {
    assert(FALSE, "Could not infer panel type")
  }
  attr(trdf, "trelliscope") <- trobj
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
