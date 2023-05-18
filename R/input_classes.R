InputClientSideStorage <- R6::R6Class("InputClientSideStorage",
  public = list(
    as_list = function() {
      as.list(private)
    }
  ),
  private = list(
    type = "localStorage"
  )
)

InputEmailFeedback <- R6::R6Class("InputEmailFeedback",
  public = list(
    initialize = function(email = NULL, vars = c()) {
      # # properties are currently only populated through pipe functions
      # self_error_msg <- function(txt)
      #   paste0("While defining email for input feedback: ", txt)
      # check_atomic(email, "email", self_error_msg)
      # check_character(email, "email", self_error_msg)
      # private$feedbackEmail <- email
      # private$includeMetaVars <- vars
    },
    set = function(x, val) {
      private[[x]] <- val
    },
    as_list = function() {
      vars <- private$includeMetaVars
      if (is.null(vars) || length(vars) == 0 || !is.character(vars))
        vars <- list()
      if (length(vars) == 1)
        vars <- I(vars)
      assert(!is.null(private$feedbackEmail),
        msg = "This display has inputs. Must provide a feedback email via
        `add_input_email()`.")
      list(
        feedbackEmail = private$feedbackEmail,
        includeMetaVars = vars
      )
    }
  ),
  private = list(
    feedbackEmail = NULL,
    includeMetaVars = c()
  )
)

Inputs <- R6::R6Class("Inputs",
  public = list(
    initialize = function() {
      private$feedbackInterface <- InputEmailFeedback$new()
      private$storageInterface <- InputClientSideStorage$new()
    },
    get = function(x) {
      private[[x]]
    },
    set = function(nm, val) {
      private[[nm]] <- val
    },
    as_list = function() {
      list(
        inputs = unname(lapply(private$inputs, function(x) x$as_list())),
        storageInterface = private$storageInterface$as_list(),
        feedbackInterface = private$feedbackInterface$as_list()
      )
    },
    as_json = function(pretty = TRUE) {
      to_json(self$as_list(), pretty = pretty)
    },
    add_input = function(obj) {
      nm <- obj$get("name")
      if (!is.null(private$inputs[[nm]])) {
        msg("Overwriting input '{nm}'")
      }
      private$inputs[[nm]] <- obj
    }
  ),
  private = list(
    inputs = NULL,
    storageInterface = NULL,
    feedbackInterface = NULL
  )
)

Input <- R6::R6Class("Input",
  public = list(
    initialize = function(name, label = name, active = TRUE, type) {
      self_error_msg <- function(txt)
        paste0("While defining a {.val ", type, "} input: ", txt)
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
    as_json = function(pretty = TRUE) {
      to_json(self$as_list(), pretty = pretty)
    },
    error_msg = function(txt) {
      type <- private$type
      paste0("While defining a {.val ", type, "} input: ", txt)
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
      check_scalar(height, "height", self$error_msg)
      check_integer(height, "height", self$error_msg)
      private$height <- height
    }
  ),
  private = list(
    height = NULL
  )
)

NumberInput <- R6::R6Class("NumberInput",
  inherit = Input,
  public = list(
    initialize = function(name, label = name, active = TRUE,
    min = NULL, max = NULL
  ) {
      super$initialize(name, label, active, type = "number")
      if (!is.null(min)) {
        check_scalar(min, "min", self$error_msg)
        check_numeric(min, "min", self$error_msg)
      }
      if (!is.null(max)) {
        check_scalar(max, "max", self$error_msg)
        check_numeric(max, "max", self$error_msg)
      }
      if (!is.null(min) && !is.null(max)) {
        assert(min < max, msg = "{.field min} must be less than {.field max}")
      }
      private$min <- min
      private$max <- max
    }
  ),
  private = list(
    min = NULL,
    max = NULL
  )
)
