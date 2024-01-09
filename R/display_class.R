#' @importFrom R6 R6Class
Display <- R6::R6Class(
  public = list(
    df_cols_ignore = NULL, # these columns won't be written to JSON
    path = NULL,
    force_plot = NULL,
    jsonp = FALSE,
    panel_options = list(),
    info_html_file = NULL,
    fidelius_pars = NULL,
    initialize = function(
      name, description, tags, keycols, path, force_plot, order, jsonp
    ) {
      if (!is.null(name)) {
        check_scalar(name, "name")
        check_character(name, "name")
        assert(nchar(name) > 0, msg = "Name cannot be empty string")
      }
      if (!is.null(description)) {
        check_scalar(description, "description")
        check_character(description, "description")
      }
      check_scalar(path, "path")
      check_character(path, "path")
      check_scalar(force_plot, "force_plot")
      check_logical(force_plot, "force_plot")
      check_character(keycols, "keycols")
      check_atomic(tags, "tags")
      check_scalar(order, "order")
      check_numeric(order, "order")
      check_scalar(jsonp, "jsonp")
      check_logical(jsonp, "jsonp")
      private$name <- name
      private$description <- description
      private$tags <- I(as.character(tags))
      private$keycols <- keycols
      private$order <- order
      self$path <- path
      self$force_plot <- force_plot
      self$jsonp <- jsonp
      private$state <- DisplayState$new()
    },
    set = function(name, val) {
      private[[name]] <- val
    },
    set_meta = function(obj, trdf) {
      obj$check_with_data(trdf)
      obj$infer_from_data(trdf)
      name <- obj$get("varname")
      if (!is.null(private$metas[[name]]))
        msg("Replacing existing meta variable definition for {name}")
      private$metas[[name]] <- obj
    },
    set_metas = function(objs, trdf) {
      for (obj in objs)
        self$set_meta(obj, trdf)
    },
    set_state = function(obj) {
      private$state <- obj
    },
    set_view = function(obj, verbose = TRUE) {
      nm <- obj$get("name")
      if (!is.null(private$views[[nm]]) && verbose) {
        msg("Overwriting view '{nm}'")
      }
      private$views[[nm]] <- obj
    },
    set_input = function(obj) {
      if (is.null(private$inputs))
        private$inputs <- Inputs$new()
      private$inputs$add_input(obj)
    },
    get = function(name) {
      private[[name]]
    },
    get_display_path = function() {
      file.path(self$path, "displays", sanitize(private$name))
    },
    get_panel_rel_dir = function(panel_name) {
      file.path("displays", sanitize(private$name), "panels",
        sanitize(panel_name))
    },
    as_list = function() {
      inputs <- NULL
      if (!is.null(private$inputs))
        inputs <- private$inputs$as_list()
      list(
        name = private$name,
        description = private$description,
        tags = private$tags,
        keycols = I(private$keycols),
        metas = unname(lapply(private$metas, function(x) x$as_list())),
        state = private$state$as_list(),
        views = unname(lapply(private$views, function(x) x$as_list())),
        inputs = inputs,
        primarypanel = private$primarypanel,
        thumbnailurl = private$thumbnailurl,
        infoOnLoad = private$infoOnLoad,
        hasCustomInfo = private$hasCustomInfo,
        order = private$order
      )
    },
    as_json = function(pretty = TRUE) {
      to_json(self$as_list(), pretty = pretty)
    },
    get_meta_names = function(trdf) {
      nms <- names(trdf)
      ignore <- c()
      for (nm in nms) {
        x <- trdf[[nm]]
        if (!(
          inherits(x, panel_classes) ||
          inherits(x, "href_vec") ||
          inherits(x, "number_vec") ||
          inherits(x, "currency_vec") ||
          is.factor(x) ||
          is.numeric(x) ||
          inherits(x, "Date") ||
          inherits(x, "POSIXct") ||
          is.atomic(x)
        )) {
          ignore <- c(ignore, nm)
        }
      }
      setdiff(nms, ignore)
    }
  ),
  private = list(
    name = NULL,
    description = NULL,
    tags = NULL,
    keycols = NULL,
    metas = list(),
    inputs = NULL,
    state = NULL,
    views = list(),
    order = 0,
    primarypanel = NULL,
    thumbnailurl = NULL,
    hasCustomInfo = FALSE,
    infoOnLoad = FALSE
  )
)
