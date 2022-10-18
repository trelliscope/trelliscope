Input <- R6::R6Class("Input",
  public = list(
    initialize = function(name, label = name, active = TRUE, type) {
      self_error_msg <- function(txt)
        paste0("While defining a '", type, "' input: ", txt)
      check_scalar(name, "name", self_error_msg)
      check_character(name, "name", self_error_msg)
      check_scalar(label, "name", self_error_msg)
      check_character(label, "name", self_error_msg)
      check_scalar(active, "active", self_error_msg)
      check_logical(active, "active", self_error_msg)
      private$name <- name
      private$label <- label
      private$active <- active
      private$type <- type
    },
    get = function(name) {
      private[[name]]
    },
    as_list = function() {
      as.list(private)
    },
    error_msg = function(txt) {
      paste0("While defining a '", private$type, "' input: ", txt)
    }
  ),
  private = list(
    name = NULL,
    label = NULL,
    active = NULL,
    type = NULL
  )
)

#? should we allow default value(s) to be selected?
RadioInput <- R6::R6Class("RadioInput",
  inherit = Input,
  public = list(
    initialize = function(name, label = name, active = TRUE,
      options
    ) {
      super$initialize(name, label, active, type = "radio")
      check_atomic_vector(options, "options", self$error_msg)
      private$options <- as.character(options)
    }
  ),
  private = list(
    options = NULL
  )
)

CheckboxInput <- R6::R6Class("CheckboxInput",
  inherit = Input,
  public = list(
    initialize = function(name, label = name, active = TRUE,
      options
    ) {
      super$initialize(name, label, active, type = "checkbox")
      check_atomic_vector(options, "options", self$error_msg)
      private$options <- as.character(options)
    }
  ),
  private = list(
    options = NULL
  )
)

SelectInput <- R6::R6Class("SelectInput",
  inherit = Input,
  public = list(
    initialize = function(name, label = name, active = TRUE,
      options
    ) {
      super$initialize(name, label, active, type = "select")
      check_atomic_vector(options, "options", self$error_msg)
      private$options <- as.character(options)
    }
  ),
  private = list(
    options = NULL
  )
)

MultiselectInput <- R6::R6Class("MultiselectInput",
  inherit = Input,
  public = list(
    initialize = function(name, label = name, active = TRUE,
      options
    ) {
      super$initialize(name, label, active, type = "multiselect")
      check_atomic_vector(options, "options", self$error_msg)
      private$options <- as.character(options)
    }
  ),
  private = list(
    options = NULL
  )
)

TextInput <- R6::R6Class("TextInput",
  inherit = Input,
  public = list(
    initialize = function(name, label = name, active = TRUE,
      width = 80, height = 3
    ) {
      super$initialize(name, label, active, type = "text")
      check_scalar(width, "width", self$error_msg)
      check_integer(width, "width", self$error_msg)
      check_scalar(height, "height", self$error_msg)
      check_integer(height, "height", self$error_msg)
      private$width <- width
      private$height <- height
    }
  ),
  private = list(
    width = NULL,
    height = NULL
  )
)

NumberInput <- R6::R6Class("NumberInput",
  inherit = Input,
  public = list(
    initialize = function(name, label = name, active = TRUE) {
      super$initialize(name, label, active, type = "number")
    }
  ),
  #? should we allow a min and max here?
  private = list()
)
