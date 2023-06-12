Meta <- R6::R6Class("Meta",
  public = list(
    initialize = function(
      type,
      varname,
      label = NULL,
      tags = NULL,
      filterable,
      sortable,
      maxnchar
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
      res$tags <- I(res$tags)
      res
    },
    set = function(name, val) {
      private[[name]] <- val
    },
    check_with_data = function(df) {
      if (!private$type %in% c("geo", "graph"))
        self$check_varname(df)
      if (!is.null(self$check_variable))
        self$check_variable(df)
      return(TRUE)
    },
    infer_from_data = function(df) {},
    cast_variable = identity
  ),
  private = list(
    varname = NULL,
    type = NULL,
    label = NULL,
    tags = NULL,
    filterable = TRUE,
    sortable = TRUE,
    maxnchar = 0
  )
)

PanelMeta <- R6::R6Class("PanelMeta",
  inherit = Meta,
  public = list(
    initialize = function(varname, label = NULL, tags = NULL,
      paneltype,
      aspect =  1,
      source
    ) {
      super$initialize(
        type = "panel",
        varname = varname,
        label = label,
        tags = tags,
        filterable = FALSE,
        sortable = FALSE
      )
      if (!is.null(aspect)) {
        check_scalar(aspect, "aspect", self$error_msg)
        check_pos_numeric(aspect, "aspect", self$error_msg)
        private$aspect <- aspect
      }
      assert(inherits(source, "PanelSource"),
        msg = "source must be a PanelSource")
      private$source <- source
      if (!is.null(paneltype)) {
        check_enum(paneltype, c("img", "iframe"), "paneltype",
          self$error_msg)
        private$paneltype <- paneltype
      }
    },
    as_list = function() {
      res <- super$as_list()
      res$source <- res$source$as_list()
      res
    },
    check_variable = function(df) {
      # TODO: check that variable is a valid URL or object
    },
    infer_from_data = function(df) {
      if (is.null(private$aspect)) {
        # TODO
        # private$aspect <- ... df[[private$varname]]
      }
      if (is.null(private$type)) {
        # browser()
        # TODO
        # private$type <- ... df[[private$varname]]
      }
    }
  ),
  private = list(
    paneltype = NULL,
    aspect = 1,
    source = NULL
  )
)

NumberMeta <- R6::R6Class("NumberMeta",
  inherit = Meta,
  public = list(
    initialize = function(varname, label = NULL, tags = NULL,
      digits = NULL,
      locale = TRUE,
      log = NULL
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
      if (!is.null(log)) {
        check_scalar(log, "log", self$error_msg)
        check_logical(log, "log", self$error_msg)
        private$log <- log
      }
    },
    check_variable = function(df) {
      check_numeric(df[[private$varname]], private$varname, self$data_error_msg)
      if (!is.null(private$log) && private$log == TRUE)
        check_pos_numeric(df[[private$varname]], private$varname,
          self$data_error_msg)
    },
    infer_from_data = function(df) {
      if (is.null(private$log)) {
        private$log <- needs_log(df[[private$varname]])
        if (private$log)
          msg("Inferred that variable '{private$varname}' should \
            be shown on log scale.")
      }
      if (is.null(private$digits)) {
        private$digits <- compute_digits(df[[private$varname]])
      }
    }
  ),
  private = list(
    digits = NULL,
    locale = TRUE,
    log = NULL
  )
)

CurrencyMeta <- R6::R6Class("CurrencyMeta",
  inherit = Meta,
  public = list(
    initialize = function(varname, label = NULL, tags = NULL,
      code = "USD",
      log = NULL,
      digits = 2
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
      if (!is.null(log)) {
        check_scalar(log, "log", self$error_msg)
        check_logical(log, "log", self$error_msg)
        private$log <- log
      }
      check_scalar(digits, "digits", self$error_msg)
      check_numeric(digits, "digits", self$error_msg)
      private$digits <- round(abs(digits))
    },
    check_variable = function(df) {
      check_numeric(df[[private$varname]], private$varname, self$data_error_msg)
      if (!is.null(private$log) && private$log == TRUE)
        check_pos_numeric(df[[private$varname]], private$varname,
          self$data_error_msg)
    },
    infer_from_data = function(df) {
      if (is.null(private$log)) {
        private$log <- needs_log(df[[private$varname]])
        if (private$log)
          msg("Inferred that variable '{private$varname}' should \
            be shown on log scale.")
      }
      if (is.null(private$digits)) {
        private$digits <- 2
      }
    }
  ),
  private = list(
    code = NULL,
    log = NULL,
    digits = 2
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
      if (any(is.na(df[[private$varname]])))
        private$levels <- c(private$levels, NA)
      check_atomic_vector(
        df[[private$varname]], private$varname, self$data_error_msg)
      check_exhaustive_levels(
        df[[private$varname]], private$levels, private$varname,
          self$data_error_msg)
    },
    cast_variable = function(df) {
      if (!is.factor(df[[private$varname]]))
        df[[private$varname]] <- factor(df[[private$varname]])
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
      idvarname,
      linkidvarname,
      labelvarname = idvarname,
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
      check_scalar(linkidvarname, "linkidvarname", self$error_msg)
      check_character(linkidvarname, "linkidvarname", self$error_msg)
      check_scalar(labelvarname, "labelvarname", self$error_msg)
      check_character(labelvarname, "labelvarname", self$error_msg)
      private$idvarname <- idvarname
      private$linkidvarname <- linkidvarname
      private$labelvarname <- labelvarname
      private$params <- empty_object()
    },
    check_variable = function(df) {
      check_has_var(df, private$idvarname, self$data_error_msg)
      check_has_var(df, private$linkidvarname, self$data_error_msg)
      check_has_var(df, private$labelvarname, self$data_error_msg)
      # TODO: clean check_graphvar up
      # check_graphvar(df[[private$varname]], private$varname,
      #   df[[private$idvarname]], private$idvarname, self$data_error_msg)
      check_not_has_var(df, private$varname, self$data_error_msg)
    }
  ),
  private = list(
    idvarname = NULL,
    linkidvarname = NULL,
    labelvarname = NULL,
    params = NULL,
    direction = NULL
  )
)

# for geometa, the inputs are the column containing latitude and the column
# containing longitude - we then create 'varname' from it, which is a list
# of the lat and long values
GeoMeta <- R6::R6Class("GeoMeta",
  inherit = Meta,
  public = list(
    initialize = function(varname, label = NULL, tags = NULL,
      latvar, longvar
      # TODO: parameters to specify how map is rendered
    ) {
      super$initialize(
        type = "geo",
        varname = varname,
        label = label,
        tags = tags,
        filterable = FALSE, # TODO: change when filters are supported
        sortable = FALSE
      )
      check_scalar(latvar, "latvar", self$error_msg)
      check_character(latvar, "latvar", self$error_msg)
      check_scalar(longvar, "longvar", self$error_msg)
      check_character(longvar, "longvar", self$error_msg)
      private$latvar <- latvar
      private$longvar <- longvar
    },
    check_variable = function(df) {
      check_has_var(df, private$latvar, self$data_error_msg)
      check_has_var(df, private$longvar, self$data_error_msg)
      check_not_has_var(df, private$varname, self$data_error_msg)
      check_latvar(df[[private$latvar]], private$latvar, self$data_error_msg)
      check_longvar(df[[private$longvar]], private$longvar, self$data_error_msg)
    },
    # we'll not create a new variable but just reference the latvar and longvar
    cast_variable = identity,
    # cast_variable = function(df) {
    #   # join into one variable and remove latvar and longvar
    #   df[[private$varname]] <- apply(
    #     df[, c(private$latvar, private$longvar)], 1,
    #     function(x) as.list(unname(x)))
    #   df[[private$latvar]] <- NULL
    #   df[[private$longvar]] <- NULL
    #   df
    # },
    # override check_varname for this because it won't exist yet
    check_varname = function(df) TRUE
  ),
  private = list(
    latvar = NULL,
    longvar = NULL
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
