# view_ws_trelliscope <- function(trdf, port = 8080) { # httpuv::randomPort()) {
#   trs <- attr(trdf, "trelliscope")

#   panel_path <- file.path(trs$get_display_path(), "panels")
#   if (!dir.exists(panel_path))
#     dir.create(panel_path, recursive = TRUE)

#   ws_get_plot <- function(row) {
#     get_plot(
#       row = as.integer(row),
#       d = trs$server$d,
#       ds = trdf,
#       plot_fn = trs$server$plot_fn,
#       key_cols = trs$get("keycols"),
#       base_path = trs$path,
#       panel_path = panel_path,
#       rel_path = trs$get_panel_rel_path(),
#       width = trs$server$width,
#       height = trs$server$height,
#       format = trs$server$format,
#       force = trs$server$force
#     )
#   }

#   s <- httpuv::startServer("127.0.0.1", port,
#     list(
#       onWSOpen = function(ws) {
#         # The ws object is a WebSocket object
#         cat("Server connection opened.\n")
#         ws$onMessage(function(binary, message) {
#           ws$send(ws_get_plot(message))
#         })
#         ws$onClose(function() {
#           cat("Server connection closed.\n")
#         })
#       },
#       staticPaths = list("/" = trs$path)
#     )
#   )
#   utils::browseURL(paste0("http://localhost:", port))
#   s
# }
