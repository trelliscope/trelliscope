
infer <- function(trdf) {
  trdf <- check_trelliscope_df(trdf)
  trdf <- infer_meta(trdf)
  trobj <- attr(trdf, "trelliscope")$clone()
  st <- trobj$get("state")
  newst <- infer_state(st, trdf, trobj$get("keycols"), trobj$get("metas"))
  trobj$set_state(newst)
  for (view in trobj$get("views")) {
    view2 <- view$clone()
    st <- view2$get("state")
    newst <- infer_state(
      st, trdf, trobj$get("keycols"), trobj$get("metas"), view2$get("name"))
    view2$set_state(newst)
    trobj$set_view(view2, verbose = FALSE)
  }
  metas <- trobj$get("metas")
  for (nm in names(metas))
    set_meta_nchar(metas[[nm]], trdf[[nm]])

  attr(trdf, "trelliscope") <- trobj
  # trdf <- infer_panel_type(trdf)
  trdf
}

infer_state <- function(state, df, keycols, metas, view = NULL) {
  view_str <- ""
  if (!is.null(view))
    view_str <- paste0(" for view '", view, "'")

  state2 <- state$clone()
  lyt <- state2$get("layout")
  if (is.null(lyt)) {
    msg("No default {.val layout} state supplied{view_str}. \\
      {.emph Using ncol=3.}")
    # TODO: maybe use nrow(df) and panel dimensions for a better initial state
    state2$set(state_layout(ncol = 3))
  }

  lbls <- state2$get("labels")
  if (is.null(lbls)) {
    msg("No default {.val labels} state supplied{view_str}. \\
      {.emph Using {paste0(keycols, collapse = ', ')}.}")
    state2$set(state_labels(keycols))
  }

  # need to add in metatype for sorts and filters
  flt <- state2$get("filter")
  for (nm in names(flt))
    flt[[nm]]$set("metatype", metas[[nm]]$get("type"))
  srt <- state2$get("sort")
  for (nm in names(srt))
    srt[[nm]]$set("metatype", metas[[nm]]$get("type"))

  # if there is a default filter that is factor, need to translate
  for (nm in names(flt)) {
    if (
      flt[[nm]]$get("filtertype") == "category" &&
      metas[[nm]]$get("type") == "factor"
    ) {
      if (length(flt[[nm]]$get("values")) > 0)
        flt[[nm]]$set(
          "values",
          I(which(metas[[nm]]$get("levels") %in% flt[[nm]]$get("values")))
        )
    }
  }

  state2
}

#' Infer meta variable definitions
#' @param trdf A trelliscope data frame created with [`as_trelliscope_df()`]
#' or a data frame which will be cast as such.
#' @export
infer_meta <- function(trdf) {
  trdf <- check_trelliscope_df(trdf)

  trobj <- attr(trdf, "trelliscope")$clone()
  def_metas <- names(trobj$get("metas"))

  needs_meta <- setdiff(names(trdf), c(def_metas, "__PANEL_KEY__"))
  # TODO: check to see if there are geo metas and ignore the lat/long varnames
  needs_removed <- character(0)

  for (nm in needs_meta) {
    cur_meta <- infer_meta_variable(trdf[[nm]], nm)
    if (is.null(cur_meta)) {
      # if (!nm %in% trobj$panel_col && !is.list(trdf[[nm]]))
      #   msg("Cannot find a data type for variable {.val {nm}}. \\
      #     This variable will not be available in the display.")
      needs_removed <- c(needs_removed, nm)
    } else {
      trdf <- add_meta_def(trdf, cur_meta)
      trobj <- attr(trdf, "trelliscope")$clone()
    }
  }
  trobj$df_cols_ignore <- needs_removed

  msg("Meta definition{?s} inferred for variable{?s} \\
    {.val {setdiff(needs_meta, needs_removed)}}")

  # finalize labels if NULL with the following priority:
  # 1. use from trobj$meta_labels if defined
  # 2. use from attr(trdf[[varname]], "label") if defined
  # 3. set it to varname
  metas <- trobj$get("metas")
  for (meta in metas) {
    curvar <- meta$get("varname")
    lbl <- NULL
    if (is.null(meta$get("label"))) {
      lbl <- attr(trdf[[curvar]], "label")
      if (is.null(lbl))
        lbl <- trobj$meta_labels[[curvar]]
      if (is.null(lbl))
        lbl <- curvar
      meta$set("label", lbl)
    }
    # also set tags if they were specified with add_meta_tags()
    if (length(meta$get("tags")) == 0 && !is.null(trobj$meta_tags[[curvar]])) {
      meta$set("tags", trobj$meta_tags[[curvar]])
    }
  }

  attr(trdf, "trelliscope") <- trobj
  trdf
}

infer_meta_variable <- function(x, nm) {
  res <- NULL
  if (is.factor(x)) {
    res <- meta_factor(nm)
  } else if (is.numeric(x)) {
    res <- meta_number(nm)
  } else if (inherits(x, "Date")) {
    res <- meta_date(nm)
  } else if (inherits(x, "POSIXct")) {
    res <- meta_datetime(nm)
  } else if (is.atomic(x)) {
    # meta_string is catch-all, although if it has a pattern that looks like
    # a URL, we can make it an href
    if (all(grepl("^(http|https):\\/\\/[^ \"]+$", x))) {
      res <- meta_href(nm)
    } else {
      res <- meta_string(nm)
    }
  }
  res
}

infer_panel_meta <- function(trdf) {
  trobj <- attr(trdf, "trelliscope") # $clone()
  metas <- trobj$get("metas")

  # if there are any list-columns with ggplot objects or htmlwidgets then
  # we need to need to add a new plot_column for them if it doesn't exist
  nms <- names(trdf)
  unlist(lapply(names(trdf), function(nm) {
    x <- trdf[[nm]]
    if (
      inherits(x, "nested_panels") ||
      (is.list(x) && any(sapply(x, inherits, c("gg", "htmlwidget"))))
    ) {
      new_nm <- paste0(nm, "_img")
      if (!new_nm %in% nms) {
        trdf[[new_nm]] <- plot_column(plot_fn = NULL, data = nm,
          by = trobj$get("keycols"), width = 600, height = 400,
          format = "png", force = FALSE)
      }
    }
  }))

  nms <- setdiff(names(trdf), names(metas))

  for (nm in nms) {
    x <- trdf[[nm]]
    if (is.character(x)) {
      http_pref <- all(grepl("^http", x))
      exts <- tolower(unique(tools::file_ext(x)))
      if (all(exts %in% valid_img_exts)) {
        ff <- x
        # need to make sure files exist locally
        if (!http_pref) {
          ff <- file.path(trobj$path, "panels", x)
          ffe <- file.exists(ff)
          nfe <- length(which(!ffe))
          if (nfe > 0)
            msg("Cannot find {nfe} file{?s} for variable {.val {nm}}. \\
              Please place them in '{trobj$path}/panels' and make this \\
              variable a vector of relative paths from there.")
          ff <- ff[ffe]
        }
        aspect <- infer_aspect_ratio(ff)
        cur_meta <- meta_panel(nm,
          type = "img", aspect = aspect, source = "file")
        trdf <- add_meta_def(trdf, cur_meta)
      } else if (all(exts == "html") || http_pref) {
        cur_meta <- meta_panel(nm,
          type = "iframe", aspect = 1.5, source = "file")
        trdf <- add_meta_def(trdf, cur_meta)
      }
    } else if (inherits(x, "plot_column")) {
      attrs <- attr(x, "plot_column")
      cur_meta <- meta_panel(nm,
        type = ifelse(attrs$format == "html", "iframe", "img"),
        aspect = attrs$height / attrs$width,
        source = ifelse(attrs$prerender, "file", "websocket"))
      trdf <- add_meta_def(trdf, cur_meta)
    }
  }

  trdf
}

infer_aspect_ratio <- function(ff) {
  if (rlang::is_installed("magick")) {
    # statistical mode of first 10 aspect ratios
    res <- unlist(lapply(head(ff, 10), function(f) {
      info <- try(magick::image_info(magick::image_read(f)), silent = TRUE)
      if (inherits(info, "try-error"))
        return(NULL)
      info$width / info$height
    }))
    if (length(res) == 0)
      return(NULL)
    return(as.numeric(names(which.max(table(res)))))
  }
  return(NULL)
}

# infer_panel_type <- function(trdf) {
#   trobj <- attr(trdf, "trelliscope")$clone()
#   pnls <- trdf[[trobj$panel_col]]
#   if (trobj$panel_col == "__server__") {
#     trobj$set("paneltype",
#       ifelse(tolower(trobj$server$format) == "html", "iframe", "img"))
#   } else if (inherits(pnls, "nested_panels")) {
#     panel1 <- pnls[[1]]
#     if (inherits(panel1, "htmlwidget")) {
#       trobj$set("paneltype", "iframe")
#     } else  {
#       trobj$set("paneltype", "img")
#     }
#   } else if (inherits(pnls, "img_panel")) {
#       trobj$set("paneltype", "img")
#       trobj$set("panelaspect", attr(pnls, "aspect_ratio"))
#       trobj$panels_written <- NA
#       trdf <- dplyr::rename(trdf, "__PANEL_KEY__" := trobj$panel_col)
#       trobj$panel_col <- "__PANEL_KEY__"
#   } else if (inherits(pnls, "iframe_panel")) {
#       trobj$set("paneltype", "iframe")
#       trobj$set("panelaspect", attr(pnls, "aspect_ratio"))
#       trobj$panels_written <- NA
#       trdf <- dplyr::rename(trdf, "__PANEL_KEY__" := trobj$panel_col)
#       trobj$panel_col <- "__PANEL_KEY__"
#   } else {
#     assert(FALSE, "Could not infer panel type")
#   }
#   attr(trdf, "trelliscope") <- trobj
#   trdf
# }

set_meta_nchar <- function(meta, x) {
  type <- meta$get("type")
  if (type %in% c("string", "factor", "date", "datetime")) {
    meta$set("maxnchar", max(nchar(as.character(x))))
  } else if (type %in% c("number", "currency")) {
    digits <- meta$get("digits")
    meta$set("maxnchar", max(nchar(format(x, nsmall = digits))))
  } else if (type == "href") {
    meta$set("maxnchar", 2)
  }
  # otherwise default of zero
}
