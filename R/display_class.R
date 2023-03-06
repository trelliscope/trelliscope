#' @importFrom R6 R6Class
Display <- R6::R6Class(
  public = list(
    df_cols_ignore = NULL, # these columns won't be written to JSON
    path = NULL,
    force_plot = NULL,
    panel_col = NULL,
    panels_written = FALSE,
    # if the user specifies meta labels using add_meta_labels(), we keep track
    # of them here so that we can apply them just before writing out the object
    meta_labels = list(),
    # if the user specifies meta tags using add_meta_tags(), we keep track
    # of them here so that we can apply them just before writing out the object
    fidelius_pars = NULL,
    meta_tags = list(),
    initialize = function(
      name, description, tags, keycols, path, force_plot, panel_col,
      keysig = NULL
    ) {
      if (!is.null(name)) {
        check_scalar(name, "name")
        check_character(name, "name")
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
      if (!is.null(keysig)) {
        check_scalar(keysig, "keysig")
        check_character(keysig, "keysig")
      }
      check_atomic(tags, "tags")
      private$name <- name
      private$description <- description
      private$tags <- I(as.character(tags))
      private$keycols <- keycols
      private$keysig <- keysig
      self$path <- path
      self$force_plot <- force_plot
      self$panel_col <- panel_col
      private$state <- DisplayState$new()
    },
    set = function(name, val) {
      private[[name]] <- val
    },
    set_meta = function(obj, trdf) {
      assert(inherits(obj, "trelliscope_meta_def"),
        msg = "Meta variable definition must come from a meta_*() function")
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
    as_list = function() {
      inputs <- NULL
      if (!is.null(private$inputs))
        inputs <- private$inputs$as_list()
      list(
        name = private$name,
        description = private$description,
        tags = private$tags,
        keycols = I(private$keycols),
        keysig = private$keysig,
        metas = unname(lapply(private$metas, function(x) x$as_list())),
        state = private$state$as_list(),
        views = unname(lapply(private$views, function(x) x$as_list())),
        inputs = inputs,
        paneltype = private$paneltype,
        panelformat = private$panelformat,
        panelaspect = private$panelaspect,
        thumbnailurl = private$thumbnailurl
      )
    },
    as_json = function(pretty = TRUE) {
      to_json(self$as_list(), pretty = pretty)
    },
    get_meta_names = function(trdf) {
      nms <- setdiff(names(trdf), "__PANEL_KEY__")
      ignore <- c()
      for (nm in nms) {
        cur_meta <- infer_meta_variable(trdf[[nm]], nm)
        if (is.null(cur_meta))
          ignore <- c(ignore, nm)
      }
      setdiff(nms, ignore)
    }
  ),
  private = list(
    name = NULL,
    description = NULL,
    tags = NULL,
    keycols = NULL,
    keysig = NULL,
    metas = list(),
    inputs = NULL,
    state = NULL,
    views = list(),
    paneltype = NULL,
    panelformat = NULL,
    panelaspect = NULL,
    thumbnailurl = NULL
  )
)
