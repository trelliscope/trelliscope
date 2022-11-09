#' @importFrom R6 R6Class
Display <- R6::R6Class(
  public = list(
    df = NULL,
    df_cols_ignore = NULL, # these columns won't be written to JSON
    path = NULL,
    force_plot = NULL,
    panel_col = NULL,
    panels_written = FALSE,
    # if the user specifies meta labels using add_meta_labels(), we keep track
    # of them here so that we can apply them just before writing out the object
    meta_labels = list(),
    initialize = function(
      df, name, description, tags, key_cols, path, force_plot, panel_col
    ) {
      assert(inherits(df, "data.frame"),
        msg = "Argument 'df' must be a data frame")
      self$df <- df
      check_scalar(name, "name")
      check_character(name, "name")
      check_scalar(description, "description")
      check_character(description, "description")
      check_scalar(path, "path")
      check_character(path, "path")
      check_scalar(force_plot, "force_plot")
      check_logical(force_plot, "force_plot")
      check_character(key_cols, "key_cols")
      check_atomic(tags, "tags")
      private$name <- name
      private$description <- description
      private$tags <- I(as.character(tags))
      private$key_cols <- key_cols
      self$path <- path
      self$force_plot <- force_plot
      self$panel_col <- panel_col
      private$state <- DisplayState$new()
    },
    set = function(name, val) {
      private[[name]] <- val
    },
    set_meta = function(obj) {
      assert(inherits(obj, "trelliscope_meta_def"),
        msg = "Meta variable definition must come from a meta_*() function")
      obj$check_with_data(self$df)
      name <- obj$get("varname")
      if (!is.null(private$metas[[name]]))
        msg("Replacing existing meta variable definition for {name}")
      private$metas[[name]] <- obj
    },
    set_metas = function(objs) {
      for (obj in objs)
        self$set_meta(obj)
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
      nm <- obj$get("name")
      if (!is.null(private$inputs[[nm]])) {
        msg("Overwriting input '{nm}'")
      } else {
        private$inputs[[nm]] <- obj
      }
    },
    get = function(name) {
      private[[name]]
    },
    get_display_path = function() {
      file.path(self$path, "displays", sanitize(private$name))
    },
    as_list = function() {
      list(
        name = private$name,
        description = private$description,
        tags = private$tags,
        key_cols = I(private$key_cols),
        metas = unname(lapply(private$metas, function(x) x$as_list())),
        state = private$state$as_list(),
        views = unname(lapply(private$views, function(x) x$as_list())),
        inputs = unname(lapply(private$inputs, function(x) x$as_list())),
        panel_type = private$panel_type
      )
    },
    as_json = function(pretty = TRUE) {
      to_json(self$as_list(), pretty = pretty)
    }
  ),
  private = list(
    name = NULL,
    description = NULL,
    tags = NULL,
    key_cols = NULL,
    metas = list(),
    inputs = list(),
    state = NULL,
    views = list(),
    panel_type = NULL
  )
)
