start_server <- function(trdf) {
  trdf <- infer(trdf)
  trobj <- attr(trdf, "trelliscope")

  idx <- which(unlist(lapply(trobj$get("metas"), function(x)
    inherits(x, "PanelMeta") &&
    inherits(x$get("source"), "LocalWebSocketPanelSource")
  )))
  if (length(idx) == 0)
    return()

  panel_opts <- trobj$panel_options

  path_list <- list()
  html_head_list <- list()
  port <- NULL
  for (i in idx) {
    mt <- trobj$get("metas")[[i]]
    nm <- mt$get("varname")
    src <- mt$get("source")
    port <- src$get_port()

    panel_path <- file.path(trobj$get_display_path(), "panels",
      sanitize(nm))
    if (!dir.exists(panel_path))
      dir.create(panel_path, recursive = TRUE)

    p <- get_panel(trdf[[nm]][[1]])
    if (inherits(p, "htmlwidget")) {
      dir.create(file.path(trobj$path, "displays", "libs"),
        showWarnings = FALSE)
      html_head_list[[nm]] <- write_htmlwidget_deps(p,
        file.path(trobj$path, "displays"), panel_path)
    }

    path_list[[nm]] <- get_panel_rel_path(trdf[[nm]], nm,
      panel_opts[[nm]]$format)
  }

  if (is.null(port)) {
    tmp <- read_jsonp(file.path(trobj$get_display_path(), "displayInfo.jsonp"),
      simplifyVector = FALSE)
    for (i in seq_along(tmp$metas)) {
      if (
        tmp$metas[[i]]$type == "panel" &&
        tmp$metas[[i]]$source$type == "localWebSocket"
      ) {
        port <- tmp$metas[[i]]$source$port
        break
      }
    }
  }

  s <- httpuv::runServer("127.0.0.1", port,
    list(
      onWSOpen = function(ws) {
        # msg("Server connection opened on port {port}...")
        ws$onMessage(function(binary, message) {
          mesg <- jsonlite::fromJSON(message)
          nm <- mesg$panelName
          idx <- which(path_list[[nm]] == mesg$panelURL)

          out_path <- file.path(trobj$get_display_path(),
            path_list[[nm]][idx])

          if (file.exists(out_path)) {
            msg("Panel {mesg$panelURL} already exists...")
            ws$send("{}")
            return()
          }

          msg("Writing panel {mesg$panelURL}...")
          p <- get_panel(trdf[[nm]][[idx]])
          popts <- panel_opts[[nm]]
          if (length(popts$width) == 0)
            popts$width <- 600
          if (length(popts$height) == 0)
            popts$height <- 400

          panel_path <- file.path(trobj$get_display_path(), "panels",
            sanitize(nm))

          write_panel(
            p,
            file = out_path,
            base_path = trobj$path,
            panel_path = panel_path,
            width = popts$width,
            height = popts$height,
            format = popts$format,
            html_head = html_head_list[[nm]]
          )

          ws$send("{}")
        })
        ws$onClose(function() {
          # msg("Server connection closed.")
        })
      },
      staticPaths = list("/" = trobj$path)
    )
  )
  s
}
