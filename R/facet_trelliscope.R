#' Add a trelliscope facet to a ggplot
#' @param facets A formula to facet the panels on. Similar to
#'   [ggplot2::facet_wrap()]'s `facets``
#' @param scales Should scales be the same (`"same"`, the default),
#'   free (`"free"`), or sliced (`"sliced"`). May provide a single string or
#'   two strings, one for the X and Y axis respectively.
#' @param add_plot_metrics Should metrics about each panel be automatically
#'   calculated? These metrics are based on the context of what is being
#'   plotted, e.g. correlation coefficient if plot is a scatterplot.
#' @param data data used for faceting. Defaults to the first layer data
#' @importFrom ggplot2 facet_wrap waiver
#' @importFrom dplyr %>%
#' @export
facet_trelliscope <- function(facets,
  scales = "same", add_plot_metrics = FALSE,
  data = ggplot2::waiver()
) {
  ret <- list(
    facets = facets,
    facet_cols = ggplot2::facet_wrap(facets)$params$facets,
    scales = scales,
    add_plot_metrics = add_plot_metrics,
    data = data
  )

  class(ret) <- "facet_trelliscope"
  ret
}

ggplot_add.facet_trelliscope <- function(object, plot, object_name) {
  attr(plot, "trelliscope") <- object[
    c("facets", "facet_cols", "scales", "add_plot_metrics", "data")]
  class(plot) <- c("facet_trelliscope", class(plot))
  return(plot)
}

panels_build <- function(x, ...) {
  UseMethod("panels_build")
}

#' Render the panels of a trelliscope display
#' @param x A ggplot object created with [facet_trelliscope()].
#' @param data_col The name of the column to store the nested data in.
#' @param panel_col The name of the column to store the rendered panels in.
#' @param unnest_cols An optional vector of extra variable names in `x`
#'   to not be nested in the data. If specified, cannot vary within the
#'   each combination of the specified facet variables.
#' @param as_plotly should the panels be written as plotly objects?
#' @param plotly_args optional named list of arguments to send to `ggplotly`
#' @param plotly_cfg optional named list of arguments to send to plotly's
#'   `config`` method.
#' @export
#' @importFrom rlang :=
#' @importFrom dplyr count across
panels_build.facet_trelliscope <- function(
  x, data_col = "data", panel_col = "panel", unnest_cols = NULL,
  as_plotly = FALSE, plotly_args = NULL, plotly_cfg = NULL
) {
  check_scalar(panel_col, "panel_col")
  check_scalar(data_col, "data_col")
  check_character(panel_col, "panel_col")
  check_character(data_col, "data_col")
  if (!is.null(unnest_cols))
    check_character(unnest_cols, "unnest_cols")

  if (as_plotly) {
    if (!requireNamespace("plotly", quietly = TRUE))
      stop("Package 'plotly' is needed for as_plotly = TRUE Please install it.",
        call. = FALSE)
  }

  attrs <- attr(x, "trelliscope")

  # remove special class
  class(x) <- setdiff(class(x), "facet_trelliscope")

  # pp <- ggplot2::ggplot_build(x)

  if (inherits(attrs$data, "waiver")) {
    # message("using data from the first layer")
    data <- x$layers[[1]]$data # first layer data
    if (inherits(data, "waiver")) {
      # retrieve plot data
      data <- x$data
    }
  } else {
    # user-supplied
    data <- attrs$data
  }

  if (is.null(data)) {
    stop("non-NULL data must be provided either in the first plot layer ",
      "or in the 'data' parameter")
  }

  # character vector of facet columns
  # TODO need to work with facet_trelliscope(~ disp < 5)
  facet_cols <- unlist(lapply(attrs$facet_cols, rlang::as_name))
  facet_cols <- setdiff(facet_cols, "~")
  if (!all(facet_cols %in% names(data))) {
    stop("All facet_trelliscope facet columns must be found in the ",
      "data being used.")
  }

  if (panel_col %in% facet_cols)
    stop("The variable panel_col='", panel_col,
      "' matches one of the facet columns. Try a different 'panel_col'.")
  if (data_col %in% facet_cols)
    stop("The variable data_col='", data_col,
      "' matches one of the facet columns. Try a different 'data_col'.")

  if (data_col %in% names(data))
    warning("A variable with name matching data_col='", data_col,
      "' exists in the data and is being overwritten", call. = FALSE)

  unnest_cols2 <- c(facet_cols, unnest_cols)
  # group by all the facets
  data <- data %>%
    dplyr::ungroup() %>%
    # dplyr::mutate(.id = row_number()) %>%
    dplyr::mutate(.id = seq_len(nrow(data))) %>%
    tidyr::nest({{ data_col }} := !dplyr::all_of(unnest_cols2)) %>%
    dplyr::ungroup()

  if (!is.null(unnest_cols)) {
    nn <- nrow(dplyr::count(data, dplyr::across(facet_cols)))
    if (nrow(data) != nn)
      stop("The values of unnest_cols='",
        paste0(unnest_cols, collapse = "', '", " must be distinct within ",
        "the values of facet_cols."))
  }

  # get ranges of all data
  scales_info <- upgrade_scales_param(attrs$scales, x$facet)
  scales_info <- add_range_info_to_scales(x, scales_info, attrs$facet_cols)

  # swaps out the data with a subset and removes the facet
  make_plot_obj <- function(dt, pos = -1) {
    q <- x
    nms <- setdiff(names(dt), data_col)
    tmp <- dt[[data_col]][[1]]
    for (nm in nms) tmp[[nm]] <- dt[[nm]]
    q$data <- tmp[, c(nms, setdiff(names(tmp), nms))]
    q <- add_trelliscope_scales(q, scales_info, show_warnings = (pos == 1))
    if (isTRUE(as_plotly)) {
      q <- do.call(plotly::ggplotly, c(list(p = q), plotly_args))
      if (!is.null(plotly_cfg))
        q <- do.call(plotly::config, c(list(p = q), plotly_cfg))
    }
    q
  }

  # TODO: use furrr and progressr if nrow(data) > N
  # pb <- progress::progress_bar$new(
  #   format = ":what [:bar] :percent :current/:total eta::eta",
  #   total = nrow(data), width = getOption("width") - 8)
  # pb$tick(0, tokens = list(what = "calculating         "))

  if (panel_col %in% names(data))
    warning("A variable with name matching panel_col='", panel_col,
      "' exists in the data and is being overwritten", call. = FALSE)
  data[[panel_col]] <- lapply(seq_len(nrow(data)), function(i) {
    make_plot_obj(data[i, ])
  })
  class(data[[panel_col]]) <- c("trelliscope_panels", "list")
  attr(data, "facet_cols") <- facet_cols

  data
}

upgrade_scales_param <- function(scales, plot_facet) {
  if (length(scales) > 2)
    stop("scales must not be longer than length 2")

  if (any(is.na(scales)) || is.null(scales) || length(scales) == 0)
    stop("scales must be a character vector of size 1 or 2")

  if (length(scales) == 1) {
    scales <- switch(scales,
      "same" = c("same", "same"),
      "free" = c("free", "free"),
      "free_x" = c("free", "same"),
      "free_y" = c("same", "free"),
      "sliced" = c("sliced", "sliced"),
      "sliced_x" = c("sliced", "same"),
      "sliced_y" = c("same", "sliced"),
      stop(
        "if scales is of length 1, it may only be one of the following values: ",
        "c('same', 'free', 'free_x', 'free_y', 'sliced', 'sliced_x', 'sliced_y')"
      )
    )
  }

  if (!all(scales %in% c("same", "free", "sliced")))
    stop("a length 2 scales parameter can only be made of 'same', 'free', or 'sliced' values")

  # sliced is not allowed for faceted columns
  if (!inherits(plot_facet, "FacetNull")) {
    for (item_val in list(list(1, "x"), list(2, "y"))) {
      if (scales[item_val[[1]]] == "sliced") {
        message(
          "If a panel is being displayed with 'facet_wrap' or 'facet_grid', ",
          "the ", item_val[[2]], " scale can not be sliced. Using 'free' instead."
        )
        scales[item_val[[1]]] <- "free"
      }
    }
  }

  list(
    x_info = list(name = "x", scale_type = scales[1]),
    y_info = list(name = "y", scale_type = scales[2]))
}

#' @importFrom utils packageVersion
#' @importFrom ggplot2 ggplot_build
#' @importFrom dplyr vars
add_range_info_to_scales <- function(plot, scales_info, facet_cols) {
  x_scale_type <- scales_info$x_info$scale_type
  y_scale_type <- scales_info$y_info$scale_type

  if (
    any(
      x_scale_type != "free",
      y_scale_type != "free"
    )
  ) {
    # get the ranges from the data
    scale_plot <- plot_clone(plot)

    scales_val <- switch(x_scale_type,
      free = switch(y_scale_type, same = "free_x", "free"),
      sliced = switch(y_scale_type, same = "free_x", "free"),
      same = switch(y_scale_type, same = "fixed", "free_y")
    )

    if (packageVersion("ggplot2") > "2.2.1") {
      facet_part <- ggplot2::facet_wrap(
        dplyr::vars(facet_cols), scales = scales_val)
    } else {
      facet_part <- ggplot2::facet_wrap(facet_cols, scales = scales_val)
    }

    if (inherits(scale_plot$facet, "FacetNull")) {
      # add a facet_wrap with scales == free and get limits
      # since can only be same here. build_plot with extra param and take limits
      facet_part$params$facets <- facet_cols

    } else {
      # can only do same (or free)
      # since can only be same here. build_plot with extra param and take limits
      facet_part$params$facets <- append(
        scale_plot$facet$params$rows,
        append(
          scale_plot$facet$params$cols,
          facet_cols
        )
      )
    }
    scale_plot <- scale_plot + facet_part

    scale_plot_built <- ggplot2::ggplot_build(scale_plot)

    calculate_scale_info <- function(scale_info, plot_scales) {
      test_scale <- plot_scales[[1]]
      scale_info$scale <- test_scale

      if (inherits(test_scale, "ScaleDiscrete")) {
        scale_info$data_type <- "discrete"

        if (scale_info$scale_type == "sliced") {
          message(
            "facet_trelliscope does not know how to handle a 'sliced' scale for discrete data. ",
            "Using 'free' type"
          )
          scale_info$scale_type <- "free"
        } else {
          # isn't free, so can take first test_scale and reutrn range values
          scale_info$levels <- test_scale$range$range
        }
      } else {
        # continuous
        scale_info$data_type <- "continuous"

        if (scale_info$scale_type == "same") {
          # test scale is accurate for all panels
          scale_info$range <- test_scale$range$range
        }

        # Behavior for relation="sliced" is similar, except that the length (max - min)
        # of the scales are constrained to remain the same across panels."
        if (scale_info$scale_type == "sliced") {
          range_list <- lapply(plot_scales, function(ps) {
            ps$range$range
          })
          diffs <- unlist(lapply(range_list, diff))

          max_diff <- diffs[which.max(diffs)]

          scale_info$width <- max_diff
        }
      }

      return(scale_info)
    }

    if (packageVersion("ggplot2") > "2.2.1") {
      scales_info$x_info <- calculate_scale_info(
        scales_info$x_info,
        scale_plot_built$layout$panel_scales_x
      )
      scales_info$y_info <- calculate_scale_info(
        scales_info$y_info,
        scale_plot_built$layout$panel_scales_y
      )
    } else {
      scales_info$x_info <- calculate_scale_info(
        scales_info$x_info,
        scale_plot_built$layout$panel_scales[[scales_info$x_info$name]]
      )
      scales_info$y_info <- calculate_scale_info(
        scales_info$y_info,
        scale_plot_built$layout$panel_scales[[scales_info$y_info$name]]
      )
    }
  }

  scales_info
}

plot_clone <- utils::getFromNamespace("plot_clone", "ggplot2")

add_trelliscope_scales <- function(p, scales_info, ...) {
  p %>%
    add_trelliscope_scale(scales_info$x_info$name, scales_info$x_info, ...) %>%
    add_trelliscope_scale(scales_info$y_info$name, scales_info$y_info, ...)
}

#' @importFrom rlang eval_tidy
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous scale_x_time
#' scale_y_time scale_x_date scale_y_date scale_x_datetime scale_y_datetime
#' scale_x_discrete scale_y_discrete
# the goal is to add a scale if a scale doesn't already exist.
# if a scale exists, we should NOT overwrite it.
add_trelliscope_scale <- function(
  p, axis_name, scale_info, show_warnings = FALSE
  ) {
  axis_scales <- p$scales$get_scales(axis_name)
  if (!is.null(axis_scales$limits)) {
    # return if there already is a limit set for this axis
    return(p)
  }

  scale_type <- scale_info$scale_type

  if (
    is.null(p$mapping[[axis_name]])
  ) {
    # this is a possibly calculated axis, leave alone
    if (
      isTRUE(show_warnings) &&
      scale_type != "free" &&
      is.null(p$scales$get_scales(axis_name))
    ) {
      # warn as it isn't a free axis
      message(
        "Axis: '", axis_name, "' is missing a global aesthetic. ",
        "Add a custom scale to change default behavior",
        call. = FALSE
      )
    }

    return(p)
  }
  if (scale_type != "free") {

    if (scale_info$data_type == "continuous") {
      # scale_fn <- switch(axis_name,
      #   "x" = scale_x_continuous,
      #   "y" = scale_y_continuous,
      # )
      #
      scale_fn <- switch(class(scale_info$scale)[1],
        "ScaleContinuousPosition" =
          switch(axis_name, "x" = scale_x_continuous, "y" = scale_y_continuous),
        "ScaleContinuousTime" =
          switch(axis_name, "x" = scale_x_time, "y" = scale_y_time),
        "ScaleContinuousDate" =
          switch(axis_name, "x" = scale_x_date, "y" = scale_y_date),
        "ScaleContinuousDatetime" =
          switch(axis_name, "x" = scale_x_datetime, "y" = scale_y_datetime)
      )

      if (scale_type == "free") {
        # "Use NA to refer to the existing minimum or maximum."
        p <- p + scale_fn(limits = c(NA, NA))

      } else if (scale_type == "same") {
        # have to make the scale and set the information manually as dates are formatted as numeric
        # p <- p + scale_fn(limits = c(NA, NA))
        scale_item <- scale_fn()
        scale_item$limits <- scale_info$range
        p <- p + scale_item

      } else if (scale_type == "sliced") {
        if (packageVersion("ggplot2") > "2.2.1") {
          dt_range <- rlang::eval_tidy(p$mapping[[axis_name]], data = p$data) %>%
            range(na.rm = TRUE)
        } else {
          dt_range <- eval(p$mapping[[axis_name]], envir = p$data) %>%
            range(na.rm = TRUE)
        }

        mid_range_val <- mean(dt_range)

        width <- scale_info$width
        limits <- c(mid_range_val - 1 / 2 * width, mid_range_val + 1 / 2 * width)

        if (!isTRUE(all.equal(dt_range, limits))) {
          # this if check is done to avoid silly R floating point rounding errors
          # this situation should only happen twice. one for each axis
          p <- p + scale_fn(limits = limits)
        }
      }
    } else if (scale_info$data_type == "discrete") {
      # data_column <- eval(p$mapping[[axis_name]], envir = p$data)

      scale_fn <- switch(axis_name,
        "x" = scale_x_discrete,
        "y" = scale_y_discrete,
      )

      if (scale_type == "free") {
        # at least have them appear in the same order
        p <- p + scale_fn(limits = scale_info$levels, drop = TRUE)
      } else if (scale_type == "same") {
        p <- p + scale_fn(limits = scale_info$levels, drop = FALSE)
      }
    }
  }

  p
}
