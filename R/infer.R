
infer <- function(disp) {
  check_display_object(disp)
  disp <- infer_meta(disp)
  disp2 <- disp$clone()
  st <- disp2$get("state")
  newst <- infer_state(st, disp$df, disp$get("keycols"))
  disp2$set_state(newst)
  for (view in disp2$get("views")) {
    view2 <- view$clone()
    st <- view2$get("state")
    newst <- infer_state(st, disp$df, disp$get("keycols"), view2$get("name"))
    view2$set_state(newst)
    disp2$set_view(view2, verbose = FALSE)
  }
  # disp2 <- infer_panel_type(disp2)
  disp2
}

infer_state <- function(state, df, keycols, view = NULL) {
  view_str <- ""
  if (!is.null(view))
    view_str <- paste0(" for view '", view, "'")

  state2 <- state$clone()
  lyt <- state2$get("layout")
  if (is.null(lyt)) {
    msg("No default {.val layout} state supplied{view_str}. \\
      {.emph Using nrow=2, ncol=3.}")
    # TODO: maybe use nrow(df) and panel dimensions for a better initial state
    state2$set(state_layout(nrow = 2, ncol = 3))
  }

  lbls <- state2$get("labels")
  if (is.null(lbls)) {
    msg("No default {.val labels} state supplied{view_str}. \\
      {.emph Using {paste0(keycols, collapse = ', ')}.}")
    state2$set(state_labels(keycols))
  }

  state2
}

#' Infer meta variable definitions
#' @param disp A trelliscope display object created with [`trelliscope()`].
#' @export
infer_meta <- function(disp) {
  check_display_object(disp)

  disp2 <- disp$clone()
  def_metas <- names(disp2$get("metas"))

  needs_meta <- setdiff(names(disp2$df), c(def_metas, "__PANEL_KEY__"))
  # TODO: check to see if there are geo metas and ignore the lat/long varnames
  needs_removed <- character(0)

  for (nm in needs_meta) {
    cur_meta <- infer_meta_variable(disp2$df[[nm]], nm)
    if (is.null(cur_meta)) {
      # if (!nm %in% disp2$panel_col && !is.list(disp2$df[[nm]]))
      #   msg("Cannot find a data type for variable {.val {nm}}. \\
      #     This variable will not be available in the display.")
      needs_removed <- c(needs_removed, nm)
    } else {
      disp2 <- add_meta_def(disp2, cur_meta)
    }
  }
  disp2$df_cols_ignore <- needs_removed

  msg("Meta definition{?s} inferred for variable{?s} \\
    {.val {setdiff(needs_meta, needs_removed)}}")

  # finalize labels if NULL with the following priority:
  # 1. use from disp$meta_labels if defined
  # 2. use from attr(disp$df[[varname]], "label") if defined
  # 3. set it to varname
  metas <- disp2$get("metas")
  for (meta in metas) {
    curvar <- meta$get("varname")
    lbl <- NULL
    if (is.null(meta$get("label"))) {
      lbl <- attr(disp2$df[[curvar]], "label")
      if (is.null(lbl))
        lbl <- disp2$meta_labels[[curvar]]
      if (is.null(lbl))
        lbl <- curvar
      meta$set("label", lbl)
    }
    # also set tags if they were specified with add_meta_tags()
    if (length(meta$get("tags")) == 0 && !is.null(disp2$meta_tags[[curvar]])) {
      meta$set("tags", disp2$meta_tags[[curvar]])
    }
  }

  disp2
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
