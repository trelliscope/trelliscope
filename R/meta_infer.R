
meta_infer <- function(disp) {
  check_display_object(disp)

  disp2 <- disp$clone()
  def_metas <- names(disp2$get("metas"))

  needs_meta <- setdiff(names(disp2$df), def_metas)
  # TODO: check to see if there are geo metas and ignore the lat/long varnames
  needs_removed <- character(0)

  for (nm in needs_meta) {
    cur_meta <- meta_infer_variable(disp2$df[[nm]], nm)
    if (is.null(cur_meta)) {
      message("Note: Cannot find a data type for variable: ", nm,
        ". This variable will not be available in the display.")
      needs_removed <- c(needs_removed, nm)
    } else {
      disp2 <- add_meta_def(disp2, cur_meta)
    }
  }
  disp2$df <- disp2$df[, setdiff(names(disp2$df), needs_removed)]

  # finalize labels if NULL with the following priority:
  # 1. use from disp$meta_labels if defined
  # 2. use from attr(disp$df[[varname]], "label") if defined
  # 3. set it to varname
  metas <- disp2$get("metas")
  for (meta in metas) {
    lbl <- NULL
    if (is.null(meta$get("label"))) {
      lbl <- attr(disp2$df[[meta$get("varname")]], "label")
      if (is.null(lbl)) {
        lbl <- disp2$meta_labels[[meta$get("varname")]]
      }
      if (is.null(lbl)) {
        lbl <- meta$get("varname")
      }
      meta$set("label", lbl)
    }
  }

  disp2
}

meta_infer_variable <- function(x, nm) {
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
