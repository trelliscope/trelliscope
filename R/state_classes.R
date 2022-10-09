.state_types <- c("label", "sort", "filter")

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
    }
  ),
  private = list(
    type = NULL
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
