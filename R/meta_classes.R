Meta <- R6::R6Class("Meta",
  public = list(
    initialize = function(
      type,
      varname,
      label = NULL,
      tags = NULL,
      filterable,
      sortable
    ) {
      private$varname <- varname
      private$label <- label
      private$type <- type
      private$filterable <- filterable
      private$sortable <- sortable
      if (is.null(tags))
        tags <- character(0)
      check_atomic(tags, "tags", self$error_msg)
      private$tags <- I(as.character(tags))
    },
    get = function(name) {
      private[[name]]
    },
    # set_value = function(el, val) {
    #   private[[el]] <- val
    # },
    error_msg = function(txt) {
      paste0("While defining a {.val ", private$type,
        "} meta variable for the variable {.val ",
        private$varname, "}: ", txt)
    },
    data_error_msg = function(txt) {
      paste0("While checking meta variable definition for variable {.val ",
        private$varname, "} against the data: ", txt)
    },
    as_list = function() {
      self$finalize()
    },
    as_json = function(pretty = TRUE) {
      to_json(self$as_list(), pretty = pretty)
    },
    check_varname = function(df) {
      check_has_var(df, private$varname, self$error_msg)
    },
    # we must have a label before it is serialized
    finalize = function() {
      res <- as.list(private)
      if (is.null(res$label))
        res$label <- res$varname
      res
    },
    set = function(name, val) {
      private[[name]] <- val
    },
    check_with_data = function(df) {
      self$check_varname(df)
      if (!is.null(self$check_variable))
        self$check_variable(df)
    },
    cast_variable = identity
  ),
  private = list(
    varname = NULL,
    type = NULL,
    label = NULL,
    tags = NULL,
    filterable = TRUE,
    sortable = TRUE
  )
)

NumberMeta <- R6::R6Class("NumberMeta",
  inherit = Meta,
  public = list(
    initialize = function(varname, label = NULL, tags = NULL,
      digits = NULL,
      locale = TRUE
    ) {
      super$initialize(
        type = "number",
        varname = varname,
        label = label,
        tags = tags,
        filterable = TRUE,
        sortable = TRUE
      )
      if (!is.null(digits)) {
        check_scalar(digits, "digits", self$error_msg)
        check_integer(digits, "digits", self$error_msg)
        private$digits <- digits
      }
      if (!is.null(locale)) {
        check_scalar(locale, "locale", self$error_msg)
        check_logical(locale, "locale", self$error_msg)
        private$locale <- locale
      }
    },
    check_variable = function(df) {
      check_numeric(df[[private$varname]], private$varname, self$data_error_msg)
    }
  ),
  private = list(
    digits = NULL,
    locale = TRUE
  )
)

CurrencyMeta <- R6::R6Class("CurrencyMeta",
  inherit = Meta,
  public = list(
    initialize = function(varname, label = NULL, tags = NULL,
      code = "USD"
    ) {
      super$initialize(
        type = "currency",
        varname = varname,
        label = label,
        tags = tags,
        filterable = TRUE,
        sortable = TRUE
      )
      if (!is.null(code)) {
        check_scalar(code, "code", self$error_msg)
        check_enum(code, unique(currencies$code_alpha), "code", self$error_msg)
        private$code <- code
      }
    },
    check_variable = function(df) {
      check_numeric(df[[private$varname]], private$varname, self$data_error_msg)
    }
  ),
  private = list(
    code = NULL
  )
)

StringMeta <- R6::R6Class("StringMeta",
  inherit = Meta,
  public = list(
    initialize = function(varname, label = NULL, tags = NULL) {
      super$initialize(
        type = "string",
        varname = varname,
        label = label,
        tags = tags,
        filterable = TRUE,
        sortable = TRUE
      )
    },
    check_variable = function(df) {
      check_atomic_vector(df[[private$varname]],
        private$varname, self$data_error_msg)
    },
    cast_variable = function(df) {
      df[[private$varname]] <- as.character(df[[private$varname]])
      df
    }
  )
)

FactorMeta <- R6::R6Class("FactorMeta",
  inherit = Meta,
  public = list(
    initialize = function(varname, label = NULL, tags = NULL,
      levels = NULL
    ) {
      super$initialize(
        type = "factor",
        varname = varname,
        label = label,
        tags = tags,
        filterable = TRUE,
        sortable = TRUE
      )
      if (!is.null(levels)) {
        check_atomic_vector(levels, "levels", self$error_msg)
        check_character(levels, "levels", self$error_msg)
        private$levels <- levels
      }
    },
    check_variable = function(df) {
      # need to infer levels if it wasn't specified already
      if (is.null(private$levels)) {
        if (is.factor(df[[private$varname]])) {
          private$levels <- levels(df[[private$varname]])
        } else {
          private$levels <- as.character(sort(unique(df[[private$varname]])))
        }
      }
      check_atomic_vector(
        df[[private$varname]], private$varname, self$data_error_msg)
      check_exhaustive_levels(
        df[[private$varname]], private$levels, private$varname,
          self$data_error_msg)
    },
    cast_variable = function(df) {
      df[[private$varname]] <- as.character(df[[private$varname]])
      df
    }
  ),
  private = list(
    levels = NULL
  )
)

DateMeta <- R6::R6Class("DateMeta",
  inherit = Meta,
  public = list(
    initialize = function(varname, label = NULL, tags = NULL) {
      super$initialize(
        type = "date",
        varname = varname,
        label = label,
        tags = tags,
        filterable = TRUE,
        sortable = TRUE
      )
    },
    check_variable = function(df) {
      check_date(df[[private$varname]], private$varname, self$data_error_msg)
    },
    cast_variable = function(df) {
      df[[private$varname]] <- as.Date(df[[private$varname]])
      df
    }
  )
)

DatetimeMeta <- R6::R6Class("DatetimeMeta",
  inherit = Meta,
  public = list(
    initialize = function(varname, label = NULL, tags = NULL,
      timezone = "UTC"
    ) {
      super$initialize(
        type = "datetime",
        varname = varname,
        label = label,
        tags = tags,
        filterable = TRUE,
        sortable = TRUE
      )
      # TODO: validate timezone
      private$timezone <- as.character(timezone)
    },
    check_variable = function(df) {
      check_datetime(df[[private$varname]], private$varname,
        self$data_error_msg)
    },
    cast_variable = function(df) {
      df[[private$varname]] <- as.POSIXct(df[[private$varname]])
      df
    }
  ),
  private = list(
    timezone = "UTC"
  )
)

GraphMeta <- R6::R6Class("GraphMeta",
  inherit = Meta,
  public = list(
    initialize = function(varname, label = NULL, tags = NULL,
      idvarname = NULL,
      direction = "none"
    ) {
      super$initialize(
        type = "graph",
        varname = varname,
        label = label,
        tags = tags,
        filterable = TRUE,
        sortable = FALSE
      )
      check_enum(direction, c("none", "to", "from"),
        private$varname, self$error_msg)
      private$direction <- as.character(direction)
      check_scalar(idvarname, "idvarname", self$error_msg)
      check_character(idvarname, "idvarname", self$error_msg)
      private$idvarname <- idvarname
    },
    check_variable = function(df) {
      check_has_var(df, private$idvarname, self$data_error_msg)
      check_graphvar(df[[private$varname]], private$varname,
        df[[private$idvarname]], private$idvarname, self$data_error_msg)
    }
  ),
  private = list(
    idvarname = NULL,
    direction = NULL
  )
)

# for geometa, the inputs are the column containing latitude and the column
# containing longitude - we then create 'varname' from it, which is a list
# of the lat and long values
GeoMeta <- R6::R6Class("GeoMeta",
  inherit = Meta,
  public = list(
    # store these in public because we won't serialize them
    latvar = NULL,
    longvar = NULL,
    initialize = function(varname, label = NULL, tags = NULL,
      latvar, longvar
      # TODO: parameters to specify how map is rendered
    ) {
      super$initialize(
        type = "geo",
        varname = varname,
        label = label,
        tags = tags,
        filterable = TRUE,
        sortable = FALSE
      )
      check_scalar(latvar, "latvar", self$error_msg)
      check_character(latvar, "latvar", self$error_msg)
      check_scalar(longvar, "longvar", self$error_msg)
      check_character(longvar, "longvar", self$error_msg)
      self$latvar <- latvar
      self$longvar <- longvar
    },
    check_variable = function(df) {
      check_has_var(df, self$latvar, self$data_error_msg)
      check_has_var(df, self$longvar, self$data_error_msg)
      check_latvar(df[[self$latvar]], self$latvar, self$data_error_msg)
      check_longvar(df[[self$longvar]], self$longvar, self$data_error_msg)
    },
    cast_variable = function(df) {
      # join into one variable and remove latvar and longvar
      df[[private$varname]] <- apply(df[, c(self$latvar, self$longvar)], 1,
        function(x) as.list(unname(x)))
      df[[self$latvar]] <- NULL
      df[[self$longvar]] <- NULL
      df
    },
    # override check_varname for this because it won't exist yet
    check_varname = function(df) TRUE
  ),
  private = list(
  )
)

HrefMeta <- R6::R6Class("HrefMeta",
  inherit = Meta,
  public = list(
    initialize = function(varname, label = NULL, tags = NULL) {
      super$initialize(
        type = "href",
        varname = varname,
        label = label,
        tags = tags,
        filterable = FALSE,
        sortable = FALSE
      )
    },
    check_variable = function(df) {
      check_atomic_vector(df[[private$varname]], private$varname,
        self$data_error_msg)
      check_character(df[[private$varname]], private$varname,
        self$data_error_msg)
    },
    cast_variable = function(df) {
      df[[private$varname]] <- as.character(df[[private$varname]])
      df
    }
  )
)

.meta_types <- list(
  number = NumberMeta,
  string = StringMeta,
  factor = FactorMeta,
  date = DateMeta,
  datetime = DatetimeMeta,
  geo = GeoMeta,
  graph = GraphMeta,
  href = HrefMeta,
  currency = CurrencyMeta
)
