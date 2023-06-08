PanelSource <- R6::R6Class("PanelSource",
  public = list(
    as_list = function() {
      as.list(private)
    },
    initialize = function(type) {
      private$type <- type
    }
  ),
  private = list(
    type = NULL
  )
)

FilePanelSource <- R6::R6Class("FilePanelSource",
  inherit = PanelSource,
  public = list(
    initialize = function() {
      super$initialize(type = "file")
    }
  ),
  private = list()
)

RESTPanelSource <- R6::R6Class("RESTPanelSource",
  inherit = PanelSource,
  public = list(
    initialize = function(url, apiKey = NULL, headers = NULL) {
      super$initialize(type = "REST")
      # TODO: validation
      private$url <- url
      private$apiKey <- apiKey
      private$headers <- headers
    }
  ),
  private = list(
    url = NULL,
    apiKey = NULL,
    headers = NULL
  )
)

LocalWebSocketPanelSource <- R6::R6Class("LocalWebSocketPanelSource",
  inherit = PanelSource,
  public = list(
    initialize = function(url, port) {
      super$initialize(type = "localWebSocket")
      # TODO: validation
      private$port <- port
    },
    get_port = function() {
      private$port
    },
    set_port = function(port) {
      private$port <- port
    }
  ),
  private = list(
    port = NULL
  )
)
