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
          msg("Replacing existing layout state specification")
        private$layout <- obj
      } else if (obj$get("type") == "labels") {
        if (!is.null(private$labels))
          msg("Replacing existing labels state specification")
        private$labels <- obj
      } else if (obj$get("type") == "sort") {
        varname <- obj$get("varname")
        if (add) {
          if (!is.null(private$sort[[varname]]))
            msg("Replacing existing sort state specification for \\
              variable {.val {varname}}")
          # make sure it is in the order we want by adding to the end
          private$sort[[varname]] <- NULL
          private$sort[[varname]] <- obj
        } else {
          if (length(private$sort) > 0)
            msg("Replacing entire existing sort specification")
          tmp <- list(obj)
          names(tmp) <- varname
          private$sort <- tmp
        }
      } else if (obj$get("type") == "filter") {
        varname <- obj$get("varname")
        if (add) {
          if (!is.null(private$filter[[varname]]))
            msg("Replacing existing filter state specification for \\
              variable {.val {varname}}")
          # make sure it is in the order we want by adding to the end
          private$filter[[varname]] <- NULL
          private$filter[[varname]] <- obj
        } else {
          if (length(private$filter) > 0)
            msg("Replacing entire existing filter specification")
          tmp <- list(obj)
          names(tmp) <- varname
          private$filter <- tmp
        }
      }
    },
    as_list = function() {
      lyt <- private$layout
      if (!is.null(lyt))
        lyt <- lyt$as_list()
      lbl <- private$labels
      if (!is.null(lbl))
        lbl <- lbl$as_list()
      list(
        layout = lyt,
        labels = lbl,
        sort = unname(lapply(private$sort, function(x) x$as_list())),
        filter = unname(lapply(private$filter, function(x) x$as_list()))
      )
    },
    as_json = function(pretty = TRUE) {
      to_json(self$as_list(), pretty = pretty)
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
    },
    set = function(name, val) {
      private[[name]] <- val
    }
    # check_with_data = function(df) {
    #   return(TRUE)
    # }
  ),
  private = list(
    type = NULL
  )
)

LayoutState <- R6::R6Class("LayoutState",
  inherit = State,
  public = list(
    initialize = function(ncol = 1, page = 1) {
      super$initialize(type = "layout")
      check_atomic_vector(ncol, "ncol", self$error_msg)
      check_integer(ncol, "ncol", self$error_msg)
      check_atomic_vector(page, "page", self$error_msg)
      check_integer(page, "page", self$error_msg)
      private$ncol <- ncol
      private$page <- page
    },
    check_with_data = function(df) {
      # TODO: could check to see if "page" makes sense after applying filters
      # and accounting for nrow and ncol
      return(TRUE)
    }
  ),
  private = list(
    ncol = NULL,
    page = 1
  )
)

LabelState <- R6::R6Class("LabelState",
  inherit = State,
  public = list(
    initialize = function(varnames) {
      super$initialize(type = "labels")
      if (is.null(varnames))
        varnames <- character(0)
      check_character(varnames, "varnames", self$error_msg)
      private$varnames <- I(varnames) # to make it a json array
    },
    check_with_data = function(df) {
      trobj <- attr(df, "trelliscope")
      inputs <- NULL
      if (!is.null(trobj$get("inputs")))
        inputs <- names(trobj$get("inputs")$get("inputs"))
      dff <- setdiff(private$varnames, c(names(df), inputs))
      assert(length(dff) == 0,
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
      assert(private$varname %in% names(df),
        msg = paste0("'", private$varname,
          "' not found in the dataset that the ", private$type,
          " state definition is being applied to"))
    },
    check_with_meta = function(meta) {
      assert(meta$get("sortable"),
        msg = self$error_msg(paste0("'", private$varname, "' is not sortable")))
    }
  ),
  private = list(
    varname = NULL,
    dir = NULL,
    metatype = NULL
  )
)

FilterState <- R6::R6Class("FilterState",
  inherit = State,
  public = list(
    applies_to = NULL, # NULL means applies to all meta definitions
    initialize = function(varname, filtertype, applies_to = NULL) {
      super$initialize(type = "filter")
      check_scalar(varname, "varname")
      check_character(varname, "varname")
      private$varname <- varname
      private$filtertype <- filtertype
      self$applies_to <- applies_to
    },
    check_with_data = function(df) {
      assert(private$varname %in% names(df),
        msg = paste0("'", private$varname,
          "' not found in the dataset that the ", private$type,
          " state definition is being applied to"))
      if (!is.null(self$extra_check))
        self$extra_check(df)
      return(TRUE)
    },
    get = function(x) private[[x]],
    check_with_meta = function(meta) {
      # # don't need this because we are in control of applying meta to filter
      # assert(private$varname == meta$get("varname"),
      #   msg = self$error_msg("filter variable name must match"))
      assert(meta$get("type") %in% self$applies_to,
        msg = self$error_msg(paste0("the meta type applied to variable '",
          private$varname, "' is not compatible with this filter")))
      # # this is redundant:
      # assert(meta$get("filterable"),
      #   msg = self$error_msg(paste0("'", private$varname,
      #   "' is not filterable")))
    }
  ),
  private = list(
    varname = NULL,
    filtertype = NULL,
    metatype = NULL
  )
)

CategoryFilterState <- R6::R6Class("CategoryFilterState",
  inherit = FilterState,
  public = list(
    initialize = function(varname, regexp = NULL, values = NULL) {
      super$initialize(varname = varname, filtertype = "category",
        applies_to = c("string", "factor"))
      if (!is.null(regexp)) {
        check_scalar(regexp, "regexp", self$error_msg)
        check_character(regexp, "regexp", self$error_msg)
      }
      if (is.null(values)) {
        values <- character(0)
      } else {
        check_character(values, "values", self$error_msg)
      }
      private$regexp <- regexp
      private$values <- I(values)
    },
    extra_check = function(df) {
      dff <- setdiff(private$values, unique(df[[private$varname]]))
      assert(length(dff) == 0,
        msg = self$data_error_msg(paste0("could not find the value(s): ",
        paste0(dff, collapse = ", "), " in the variable '",
        private$varname, "'")))
      #? could validate that regex is a valid regex as well...
    }
  ),
  private = list(
    regexp = NULL,
    values = NULL
  )
)

RangeFilterState <- R6::R6Class("RangeFilterState",
  inherit = FilterState,
  public = list(
    initialize = function(varname, filtertype, applies_to,
      min = NULL, max = NULL
    ) {
      super$initialize(varname = varname, filtertype = filtertype,
        applies_to = applies_to)
      if (!is.null(min))
        check_scalar(min, "min", self$error_msg)
      if (!is.null(max))
        check_scalar(max, "max", self$error_msg)
      private$min <- min
      private$max <- max
    }
  ),
  private = list(
    min = NULL,
    max = NULL
  )
)

NumberRangeFilterState <- R6::R6Class("NumberRangeFilterState",
  inherit = RangeFilterState,
  public = list(
    initialize = function(varname, min = NULL, max = NULL) {
      super$initialize(varname, filtertype = "numberrange",
        applies_to = "number", min = min, max = max)
      if (!is.null(min))
        check_numeric(min, "min", self$error_msg)
      if (!is.null(max))
        check_numeric(max, "max", self$error_msg)
      private$metatype <- "number"
    }
  )
)

DateRangeFilterState <- R6::R6Class("DateRangeFilterState",
  inherit = RangeFilterState,
  public = list(
    initialize = function(varname, min = NULL, max = NULL) {
      super$initialize(varname, filtertype = "daterange",
        applies_to = "date", min = min, max = max)
      if (!is.null(min))
        check_date(min, "min", self$error_msg)
      if (!is.null(max))
        check_date(max, "max", self$error_msg)
      private$metatype <- "date"
    }
  )
)

DatetimeRangeFilterState <- R6::R6Class("DatetimeRangeFilterState",
  inherit = RangeFilterState,
  public = list(
    initialize = function(varname, min = NULL, max = NULL) {
      super$initialize(varname, filtertype = "datetimerange",
        applies_to = "datetime", min = min, max = max)
      if (!is.null(min))
        check_datetime(min, "min", self$error_msg)
      if (!is.null(max))
        check_datetime(max, "max", self$error_msg)
      private$metatype <- "datetime"
    }
  )
)

# GraphFilterState
