#' @export
print.trelliscope <- function(
  x, ..., n = getOption("trs_max_print", default = 10)
) {
  print_panels(x)
  if (n > 0) {
    if (inherits(x, "tbl_df")) {
      NextMethod()
    } else {
      y <- x
      if (nrow(y) > n) {
        cat(paste("First", n, "features:\n"))
        y <- x[1:n, , drop = FALSE]
      }
      print.data.frame(y, ...)
    }
  }
  invisible(x)
}

print_panels <- function(x) {
  trobj <- attr(x, "trelliscope")
  pnls <- names(x)[unlist(lapply(x, function(x)
    inherits(x, panel_classes)))]
  mnc <- max(nchar(pnls))
  mnc2 <- max(nchar(panel_classes)) - 4 # _vec
  msg("Trelliscope data frame: {.val {trobj$get('name')}} \\
    located at {.val {trobj$path}} \\
    with {.val {length(pnls)}} panel{?s}:")
  cli::cli_ul()
  for (nm in pnls) {
    a <- x[[nm]]
    type <- if (inherits(a, "ggpanel_vec")) {
      "ggpanel"
    } else if (inherits(a, "panel_lazy_vec")) {
      "panel_lazy"
    } else if (inherits(a, "panel_local_vec")) {
      "panel_local"
    } else if (inherits(a, "panel_url_vec")) {
      "panel_url"
    } else {
      "unknown"
    }
    opts <- trobj$panel_options[[nm]]
    optstr <- cli::col_grey("[no panel options specified]")
    if (!is.null(opts)) {
      if (inherits(a, panel_lazy_classes)) {
        optstr <- "[{.field width={opts$width}}, \\
          {.field height={opts$height}}, {.field format={opts$format}}]"
      } else {
        optstr <- "[{.field aspect={opts$aspect}}]"
      }
    }
    spc <- paste(rep("\u00a0", mnc - nchar(nm)), collapse = "")
    spc2 <- paste(rep("\u00a0", mnc2 - nchar(type)), collapse = "")
    cli::cli_li(paste0("{.val {nm}}{spc}: <{type}> {spc2}", optstr))
  }
  cli::cli_end()

}

#' View trelliscope info of a trelliscope data frame
#' @param trdf A trelliscope data frame created with [`as_trelliscope_df()`]
#' or a data frame which will be cast as such.
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' panel_dat <- (
#'   ggplot(gapminder, aes(year, lifeExp)) +
#'     geom_point() +
#'     facet_panels(~country + continent)
#'   ) |>
#'   nest_panels()
#'
#' trell <- panel_dat |>
#'   as_trelliscope_df(name = "life expectancy", path = "gapminder") |>
#'   set_default_layout(nrow = 2, ncol = 4)
#'
#' show_info(trell)
#' }
#'
#' @export
show_info <- function(trdf) {
  trobj <- attr(trdf, "trelliscope")
  cli::cli_bullets(c(
    "A trelliscope display",
    "*" = "{.strong Name}: {.val {trobj$get('name')}}",
    "*" = "{.strong Description}: {.val {trobj$get('description')}}",
    "*" = ifelse(length(trobj$get("tags")) == 0,
      "{.strong Tags}: {.emph none}",
      "{.strong Tags}: {.val {trobj$get('tags')}}"
    ),
    "*" = "{.strong Key columns}: {.val {trobj$get('keycols')}}"
  ))
  cli::cli_div(theme = list(.val = list(color = "darkgray")))
  cli::cli_bullets(c("*" = "{.strong Path}: {.val {trobj$path}}"))
  cli::cli_end()
  cli::cli_bullets(
    c("*" = "{.strong Number of panels}: {.val {nrow(trdf)}}"))
  # print_meta_info_df(trobj$get("metas"), trdf,
  #   trobj$meta_labels, trobj$meta_tags)
}

#' @importFrom cli cli_bullets cli_code cli_div cli_end
#' @importFrom dplyr tibble bind_rows
#' @importFrom utils capture.output
cli_print_tbl <- function(x) {
  nr <- nrow(x)
  a <- utils::capture.output(print(x, n = 8, width = getOption("width") - 4))
  a <- a[-1]
  a <- gsub("^[0-9] ", "", a)
  a <- gsub("^# ", "", a)
  a <- gsub("^ +", "", a)
  lns <- paste(rep("\u2500", nchar(a[1])), collapse = "")
  a[2] <- lns
  a <- c(lns, a, lns)
  a <- gsub("^", "    ", a)
  if (nr > 8)
    a <- a[-length(a)]
  cli::cli_div(theme = list(.code = list(color = "darkgray")))
  cli::cli_code(a, language = NULL)
  cli::cli_end()
}

print_meta_info_df <- function(metas, df, meta_labels, meta_tags) {
  # TODO: revamp this
  # def_metas <- names(metas)
  # needs_meta <- setdiff(names(df), c(def_metas, "__PANEL_KEY__"))

  # if (length(metas) > 0) {
  #   mt <- lapply(metas, function(x) {
  #     nm <- x$get("varname")
  #     # TODO: label value needs to match logic in infer.R
  #     lbl <- x$get("label")
  #     if (!is.null(meta_labels[[nm]]))
  #       lbl <- meta_labels[[nm]]
  #     nc <- nchar(lbl)
  #     lbl <- substr(lbl, 1, 25)
  #     if (nchar(lbl) != nc)
  #       lbl <- paste0(lbl, "\u2026")
  #     tags <- x$get("tags")
  #     if (length(tags) == 0 && length(meta_tags[[nm]] > 0))
  #       tags <- meta_tags[[nm]]
  #     if (length(tags) == 0) {
  #       tags <- "[]"
  #     } else {
  #       tags <- paste0(tags, collapse = ", ")
  #     }
  #     nc <- nchar(tags)
  #     tags <- substr(tags, 1, 25)
  #     if (nchar(tags) != nc)
  #       tags <- paste0(tags, "\u2026")
  #     dplyr::tibble(
  #       name = nm,
  #       type = x$get("type"),
  #       label = lbl,
  #       tags = tags
  #     )
  #   }) |>
  #   dplyr::bind_rows()

  #   cli::cli_bullets(c("*" = "Defined metadata variables:"))
  #   cli_print_tbl(mt)
  # }

  # needs_removed <- character(0)
  # nmt <- list()
  # for (nm in needs_meta) {
  #   cur_meta <- infer_meta_variable(df[[nm]], nm)
  #   if (is.null(cur_meta)) {
  #     needs_removed <- c(needs_removed, nm)
  #   } else {
  #     lbl <- "[none]"
  #     if (!is.null(meta_labels[[nm]]))
  #       lbl <- meta_labels[[nm]]
  #     nmt <- c(nmt, list(tibble(
  #       name = nm,
  #       "inferred type" = cur_meta$get("type"),
  #       label = lbl
  #     )))
  #   }
  # }

  # if (length(nmt) > 0) {
  #   nmt <- bind_rows(nmt)
  #   cli::cli_bullets(c("*" = "Metadata variables that will be inferred:"))
  #   cli_print_tbl(nmt)
  # }

  # if (length(needs_removed) > 0)
  #   cli::cli_bullets(c("*" = "Variables that will be ignored as metadata:
  #     {.val {needs_removed}}"))
}

  # cfm <- function(x, envir = parent.frame)
  #   cli_format_method(cli_text(x, .envir = envir))

  # names <- c(cfm("{.strong name}"), rep("", length(metas)))
  # labels <- c(cfm("{.strong label}"), rep("", length(metas)))
  # types <- c(cfm("{.strong type}"), rep("", length(metas)))
  # tags <- c(cfm("{.strong tags}"), rep("", length(metas)))

  # for (ii in seq_along(metas)) {
  #   x <- metas[[ii]]
  #   names[ii + 1] <- x$get("varname")
  #   types[ii + 1] <- x$get("type")
  #   labels[ii + 1] <- x$get("label")
  #   tags[ii + 1] <- paste0(x$get("tags"), collapse = ", ")
  # }

  # fr <- function(a)
  #   ansi_align(a, max(ansi_nchar(a)), "left")

  # glue::glue("  {fr(names)}  {fr(types)}  {fr(tags)}")
