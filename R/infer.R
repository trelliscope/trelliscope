
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
  # TODO: make this part of infer_meta
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
# @export
infer_meta <- function(trdf) {
  trdf <- check_trelliscope_df(trdf)

  panel_cols <- names(which(
    unlist(lapply(trdf, function(x) inherits(x, panel_classes)))))
  panel_opts <- attr(trdf, "trelliscope")$panel_options
  needs_opts <- setdiff(panel_cols, names(panel_opts))
  new_opts <- lapply(needs_opts, function(nm) {
    if (inherits(trdf[[nm]], panel_lazy_classes)) {
      panel_options_lazy()
    } else {
      panel_options()
    }
  })
  names(new_opts) <- needs_opts
  trdf <- do.call(set_panel_options, c(list(trdf = trdf), new_opts))

  trobj <- attr(trdf, "trelliscope")$clone()

  # all meta info is defined through the variable attributes and labels and tags
  needs_removed <- c()
  for (nm in names(trdf)) {
    cur_meta <- infer_meta_variable(trdf[[nm]], nm, trobj$panel_options[[nm]])
    if (is.null(cur_meta)) {
      # if (!nm %in% trobj$panel_col && !is.list(trdf[[nm]]))
      #   msg("Cannot find a data type for variable {.val {nm}}. \\
      #     This variable will not be available in the display.")
      needs_removed <- c(needs_removed, nm)
    } else {
      trobj$set_meta(cur_meta, trdf)
    }
  }
  trobj$df_cols_ignore <- needs_removed

  if (length(needs_removed) > 0)
    msg("Meta definition{?s} not created for variable{?s} \\
    {.val {needs_removed}}")

  attr(trdf, "trelliscope") <- trobj
  trdf
}

infer_meta_variable <- function(x, nm, panel_opts) {
  label <- attr(x, "label")
  tags <- attr(x, "tags")

  res <- NULL
  if (inherits(x, panel_classes)) {
    if (inherits(x, panel_lazy_classes) && panel_opts$prerender == FALSE) {
      psource <- LocalWebSocketPanelSource$new(port = NULL)
      aspect <- panel_opts$width / panel_opts$height
    } else {
      psource <- FilePanelSource$new()
      aspect <- panel_opts$aspect
      if (is.null(aspect))
        aspect <- infer_aspect_ratio(x)
    }

    res <- PanelMeta$new(
      varname = nm,
      label = label,
      tags = tags,
      paneltype = panel_opts$type,
      aspect = aspect,
      source = psource
    )
  } else if (inherits(x, "href_vec")) {
    res <- HrefMeta$new(
      varname = nm,
      label = label,
      tags = tags
    )
  } else if (inherits(x, "number_vec")) {
    res <- NumberMeta$new(
      varname = nm,
      label = label,
      tags = tags,
      digits = attr(x, "digits"),
      locale = attr(x, "locale"),
      log = attr(x, "log")
    )
  } else if (inherits(x, "currency_vec")) {
    res <- CurrencyMeta$new(
      varname = nm,
      label = label,
      tags = tags,
      digits = attr(x, "digits"),
      code = attr(x, "code"),
      log = attr(x, "log")
    )
  } else if (is.factor(x)) {
    res <- FactorMeta$new(
      varname = nm,
      label = label,
      tags = tags,
      levels = levels(x)
    )
  } else if (is.numeric(x)) {
    res <- NumberMeta$new(
      varname = nm,
      label = label,
      tags = tags,
      digits = attr(x, "digits"),
      locale = attr(x, "locale"),
      log = attr(x, "log")
    )
  } else if (inherits(x, "Date")) {
    res <- DateMeta$new(
      varname = nm,
      label = label,
      tags = tags
    )
  } else if (inherits(x, "POSIXct")) {
    res <- DatetimeMeta$new(
      varname = nm,
      label = label,
      tags = tags
    )
  } else if (is.atomic(x)) {
    # meta_string is catch-all, although if it has a pattern that looks like
    # a URL, we can make it an href
    if (all(grepl("^(http|https):\\/\\/[^ \"]+$", x))) {
      res <- HrefMeta$new(
        varname = nm,
        label = label,
        tags = tags
      )
    } else {
      res <- StringMeta$new(
        varname = nm,
        label = label,
        tags = tags
      )
    }
  }

  res
}

infer_aspect_ratio <- function(ff) {
  if (rlang::is_installed("magick")) {
    # statistical mode of first 5 aspect ratios
    res <- unlist(lapply(head(ff, 5), function(f) {
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
