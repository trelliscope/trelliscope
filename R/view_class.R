View <- R6::R6Class("View",
  public = list(
    initialize = function(
      name,
      ...
    ) {
      check_scalar(name)
      check_character(name)
      # trelliscope_state_def
      private$name <- name
      objs <- list(...)
      types <- unlist(lapply(objs, function(x) {
        check_state_object(x)
        x$get("type")
      }))

      lyt_idx <- which(types == "layout")
      lyt <- NULL
      if (length(lyt_idx) == 0) {
        message("No layout definition supplied for view '", name, "'. ",
          "Using the default layout.")
      } else if (length(lyt_idx) > 1) {
        message("Multiple layout definitions supplied for view '", name, "'. ",
          "Using the last-defined definition.")
        lyt <- objs[[tail(lyt_idx, 1)]]
      } else {
        lyt <- objs[[lyt_idx]]
      }

      lbl_idx <- which(types == "labels")
      lbl <- NULL
      if (length(lbl_idx) == 0) {
        message("No labels definition supplied for view '", name, "'. ",
          "Using default labels.")
      } else if (length(lbl_idx) > 1) {
        message("Multiple labels definitions supplied for view '", name, "'. ",
          "Using the last-defined definition.")
        lbl <- objs[[tail(lbl_idx, 1)]]
      } else {
        lbl <- objs[[lbl_idx]]
      }

      sort_idx <- which(types == "sort")
      sorts <- NULL
      if (length(sort_idx) == 0) {
        message("No sort definitions supplied for view '", name, "'. ",
          "Using default sort.")
      } else {
        sorts <- objs[sort_idx]
      }

      filter_idx <- which(types == "filter")
      filters <- NULL
      if (length(filter_idx) == 0) {
        message("No filter definitions supplied for view '", name, "'.")
      } else {
        filters <- objs[filter_idx]
      }

      state <- DisplayState$new()
      if (!is.null(lyt))
        state$set(lyt)
      if (!is.null(lbl))
        state$set(lbl)
      for (obj in sorts) {
        state$set(obj, add = TRUE)
      }
      for (obj in filters) {
        state$set(obj, add = TRUE)
      }
      private$states <- state
    },
    get = function(name) {
      private[[name]]
    },
    # as_list = function() {
    #   as.list(private)
    # },
    # as_json = function(pretty = FALSE) {
    #   to_json(as.list(private))
    # },
    check_with_data = function(df) {
      if (!is.null(private$states$get("layout")))
        private$states$get("layout")$check_with_data(df)
      if (!is.null(private$states$get("labels")))
        private$states$get("labels")$check_with_data(df)
      for (obj in private$states$get("sort"))
        obj$check_with_data(df)
      for (obj in private$states$get("filter"))
        obj$check_with_data(df)
    }
    # infer_missing_states = function(df) {
    #   TODO
    # }
  ),
  private = list(
    name = NULL,
    states = NULL
  )
)
