.state_types <- c("label", "sort", "filter")

# contains the collection of all states necessary to define a display
DisplayState <- R6::R6Class("DisplayState",
  public = list(
    get = function(name) {
      private[[name]]
    },
    set = function(obj, add = FALSE) {
      if (obj$get("type") == "layout") {
        if (!is.null(private$layout))
          message("Replacing existing layout state specification")
        private$layout <- obj
      } else if (obj$get("type") == "labels") {
        if (!is.null(private$labels))
          message("Replacing existing labels state specification")
        private$labels <- obj
      } else if (obj$get("type") == "sort") {
        varname <- obj$get("varname")
        if (add) {
          if (!is.null(private$sort[[varname]]))
            message("Replacing existing sort state specification for ",
              " variable ", varname)
          # make sure it is in the order we want by adding to the end
          private$sort[[varname]] <- NULL
          private$sort[[varname]] <- obj
        } else {
          if (length(private$sort) > 0)
            message("Replacing entire existing sort specification")
          tmp <- list(obj)
          names(tmp) <- varname
          private$sort <- tmp
        }
      } else if (obj$get("type") == "filter") {
        message("TODO")
      }
    }
  ),
  private = list(
    layout = NULL,
    labels = NULL,
    sort = list(),
    filter = list()
  )
)

State <- R6::R6Class("State",
  public = list(
    initialize = function(type) {
      private$type <- type
    },
    as_list = function() {
      as.list(private)
    },
    as_json = function(pretty = FALSE) {
      to_json(as.list(private))
    },
    error_msg = function(txt) {
      paste0("While checking a ", private$type, " state definition: ", txt)
    },
    data_error_msg = function(txt) {
      paste0("While checking ", private$type,
        " state definition against the data: ", txt)
    },
    get = function(name) {
      private[[name]]
    }
  ),
  private = list(
    type = NULL
  )
)

LayoutState <- R6::R6Class("LayoutState",
  inherit = State,
  public = list(
    initialize = function(nrow = 1, ncol = 1, arrange = "rows", page = 1) {
      super$initialize(type = "layout")
      check_atomic_vector(nrow, "nrow", self$error_msg)
      check_integer(nrow, "nrow", self$error_msg)
      check_atomic_vector(ncol, "ncol", self$error_msg)
      check_integer(ncol, "ncol", self$error_msg)
      check_atomic_vector(arrange, "arrange", self$error_msg)
      check_enum(arrange, c("rows", "cols"), "arrange", self$error_msg)
      check_atomic_vector(page, "page", self$error_msg)
      check_integer(page, "page", self$error_msg)
      private$nrow <- nrow
      private$ncol <- ncol
      private$arrange <- arrange
      private$page <- page
    },
    check_with_data = function(df) {
      # TODO: could check to see if "page" makes sense after applying filters
      # and accounting for nrow and ncol
      return(TRUE)
    }
  ),
  private = list(
    nrow = NULL,
    ncol = NULL,
    arrange = NULL,
    page = 1
  )
)

LabelState <- R6::R6Class("LabelState",
  inherit = State,
  public = list(
    initialize = function(varnames) {
      super$initialize(type = "labels")
      check_atomic_vector(varnames, "varnames", self$error_msg)
      check_character(varnames, "varnames", self$error_msg)
      private$varnames <- I(varnames) # to make it a json array
    },
    check_with_data = function(df) {
      dff <- setdiff(private$varnames, names(df))
      assertthat::assert_that(length(dff) == 0,
        msg = self$data_error_msg(paste0("Label variables not found in data: ",
          paste0(dff, collapse = ", "))))
    }
  ),
  private = list(
    varnames = NULL
  )
)

SortState <- R6::R6Class("SortState",
  inherit = State,
  public = list(
    initialize = function(varname, dir = "asc") {
      super$initialize(type = "sort")
      check_scalar(varname, "varname", self$error_msg)
      check_character(varname, "varname", self$error_msg)
      check_scalar(dir, "dir", self$error_msg)
      check_character(dir, "dir", self$error_msg)
      check_enum(dir, c("asc", "desc"), "dir", self$error_msg)
      private$varname <- varname
      private$dir <- dir
    },
    check_with_data = function(df) {
      assertthat::assert_that(private$varname %in% names(df),
        msg = paste0("'", private$varname,
          "' not found in the dataset that the ", private$type,
          " state definition is being applied to"))
    },
    check_with_meta = function(meta) {
      assertthat::assert_that(meta$get("sortable"),
        msg = self$error_msg(paste0("'", private$varname, "' is not sortable")))
    }
  ),
  private = list(
    varname = NULL,
    dir = NULL
  )
)

# FilterState <- R6::R6Class("FilterState",
#   inherit = State,
#   public = list(
#     initialize = function(varname, applies_to = NULL) {
#       super$initialize(type = "filter")
#       check_scalar(varname, "varname")
#       check_character(varname, "varname")
#       private$varname <- varname
#       private$applies_to <- applies_to
#     },
#     check_with_data = function(df) {
#       assertthat::assert_that(private$varname %in% names(df),
#         msg = paste0("'", private$varname,
#           "' not found in the dataset that the ", private$type,
#           " state definition is being applied to"))
#     },
#     check_with_meta = function(meta) {
#       assertthat::assert_that(meta$get(type) %in% applies_to)
#       assertthat::assert_that(meta$get(filterable),
#         msg = self$error_msg(paste0("'", private$varname,
#         "' is not filterable")))
#     }
#   ),
#   private = list(
#     varname = NULL,
#     applies_to = NULL # NULL means applies to all meta definitions
#   )
# )

# FilterState

# StringFilterState

# FactorFilterState

# GraphFilterState

# NumberFilterState
