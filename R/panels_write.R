write_panels <- function(trdf, nm, force = FALSE) {
  trobj <- attr(trdf, "trelliscope")
  app_path <- trobj$path

  pnls <- trdf[[nm]]
  panel_opts <- trobj$panel_options[[nm]]

  if (inherits(pnls, panel_lazy_classes)) {
    p <- get_panel(pnls[[1]])
    html_head <- NULL
    if (inherits(p, "htmlwidget")) {
      dir.create(file.path(app_path, "displays", "libs"), showWarnings = FALSE)
      html_head <- write_htmlwidget_deps(p,
        file.path(app_path, "displays"), panel_path)
    }

    panel_path <- file.path(trobj$get_display_path(), "panels",
      sanitize(nm))
    if (!dir.exists(panel_path))
      dir.create(panel_path, recursive = TRUE)

    pths <- file.path(trobj$get_display_path(),
      get_panel_rel_path(pnls, nm, panel_opts$format))

    if (panel_opts$force || force) {
      idxs <- seq_along(pths)
    } else {
      idxs <- which(!file.exists(pths))
    }

    cli::cli_progress_bar(paste("Writing", nm), total = length(idxs))

    for (idx in idxs) {
      cli::cli_progress_update()

      p <- get_panel(pnls[[idx]])

      write_panel(
        p,
        file = pths[idx],
        base_path = app_path,
        panel_path = panel_path,
        width = panel_opts$width,
        height = panel_opts$height,
        format = panel_opts$format,
        html_head = html_head
      )
    }

    cli::cli_progress_done()
  } else if (inherits(pnls, "panel_local_vec")) {
    pths <- file.path(trobj$get_display_path(),
      get_panel_rel_path(pnls, nm))

    idxs <- file.exists(pnls)
    if (!force)
      idxs <- idxs & !file.exists(pths)
    idxs <- which(idxs)

    if (length(idxs) > 0) {
      sz_est <- mean(file.info(head(pnls[idxs], 5))$size) *
        length(idxs) / 1e6
      # TODO: if sz_est is greater than 100MB, ask user if they
      # want to continue
      file.copy(pnls[idxs], pths[idxs], overwrite = TRUE)
    }
  }

  # panel_path <- file.path(trobj$get_display_path(), "panels")
  # if (inherits(pnls, "nested_panels")) {
  #   ff <- list.files(panel_path)
  #   ff <- tools::file_path_sans_ext(ff)
  #   keys <- apply(trdf[trobj$get("keycols")], 1,
  #     function(x) sanitize(paste(x, collapse = "_")))
  #   extra <- setdiff(keys, ff)
  #   assert(length(extra) == 0,
  #     msg = paste0("Found ", length(extra), " panel keys that do not have ",
  #       " a corresponding panel file."))
  # }

  # trdf
}
