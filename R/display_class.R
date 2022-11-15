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
    },
    print = function() {
      cli::cli_bullets(c(
        "A trelliscope display",
        "*" = "{.strong Name}: {.val {private$name}}",
        "*" = "{.strong Description}: {.val {private$description}}",
        "*" = ifelse(length(private$tags) == 0,
          "{.strong Tags}: {.emph none}",
          "{.strong Tags}: {.val {private$tags}}"
        ),
        "*" = "{.strong Key columns}: {.val {private$key_cols}}"
      ))
      cli::cli_div(theme = list(.val = list(color = "darkgray")))
      cli::cli_bullets(c("*" = "{.strong Path}: {.val {self$path}}"))
      cli::cli_end()
      cli::cli_bullets(
        c("*" = "{.strong Number of panels}: {.val {nrow(self$df)}}"))
      wrt <- ifelse(self$panels_written, "yes", "no")
      cli::cli_bullets(c("*" = "{.strong Panels written}: {.emph {wrt}}"))
      print_meta_info_df(self$get("metas"), self$df, self$meta_labels)
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

#' @importFrom cli cli_bullets cli_code cli_div cli_end
#' @importFrom dplyr tibble bind_rows
#' @importFrom utils capture.output
cli_print_tbl <- function(x) {
  nr <- nrow(x)
  a <- utils::capture.output(print(x, n = 8, width = getOption("width") - 4))
  a <- a[-1]
  a <- gsub("^[0-9] ", "", a)
  a <- gsub("^# ", "", a)
  a <- gsub("^ +", "", a)
  lns <- paste(rep("\u2500", nchar(a[1])), collapse = "")
  a[2] <- lns
  a <- c(lns, a, lns)
  a <- gsub("^", "    ", a)
  if (nr > 8)
    a <- a[-length(a)]
  cli::cli_div(theme = list(.code = list(color = "darkgray")))
  cli::cli_code(a, language = NULL)
  cli::cli_end()
}

print_meta_info_df <- function(metas, df, meta_labels) {
  def_metas <- names(metas)
  needs_meta <- setdiff(names(df), def_metas)

  if (length(metas) > 0) {
    mt <- lapply(metas, function(x) {
      nm <- x$get("varname")
      lbl <- x$get("label")
      if (!is.null(meta_labels[[nm]]))
        lbl <- meta_labels[[nm]]
      nc <- nchar(lbl)
      lbl <- substr(lbl, 1, 20)
      if (nchar(lbl) != nc)
        lbl <- paste0(lbl, "\u2026")
      tags <- x$get("tags")
      if (length(tags) == 0) {
        tags <- "[]"
      } else {
        paste0(tags, collapse = ", ")
      }
      dplyr::tibble(
        name = nm,
        type = x$get("type"),
        label = lbl,
        tags = tags
      )
    }) |>
    dplyr::bind_rows()

    cli::cli_bullets(c("*" = "Defined metadata variables:"))
    cli_print_tbl(mt)
  }

  needs_removed <- character(0)
  nmt <- list()
  for (nm in needs_meta) {
    cur_meta <- infer_meta_variable(df[[nm]], nm)
    if (is.null(cur_meta)) {
      needs_removed <- c(needs_removed, nm)
    } else {
      lbl <- "[none]"
      if (!is.null(meta_labels[[nm]]))
        lbl <- meta_labels[[nm]]
      nmt <- c(nmt, list(tibble(
        name = nm,
        "inferred type" = cur_meta$get("type"),
        label = lbl
      )))
    }
  }

  if (length(nmt) > 0) {
    nmt <- bind_rows(nmt)
    cli::cli_bullets(c("*" = "Metadata variables that will be inferred:"))
    cli_print_tbl(nmt)
  }

  if (length(needs_removed) > 0)
    cli::cli_bullets(c("*" = "Variables that will be ignored as metadata:
      {.val {needs_removed}}"))
}

  # cfm <- function(x, envir = parent.frame)
  #   cli_format_method(cli_text(x, .envir = envir))

  # names <- c(cfm("{.strong name}"), rep("", length(metas)))
  # labels <- c(cfm("{.strong label}"), rep("", length(metas)))
  # types <- c(cfm("{.strong type}"), rep("", length(metas)))
  # tags <- c(cfm("{.strong tags}"), rep("", length(metas)))

  # for (ii in seq_along(metas)) {
  #   x <- metas[[ii]]
  #   names[ii + 1] <- x$get("varname")
  #   types[ii + 1] <- x$get("type")
  #   labels[ii + 1] <- x$get("label")
  #   tags[ii + 1] <- paste0(x$get("tags"), collapse = ", ")
  # }

  # fr <- function(a)
  #   ansi_align(a, max(ansi_nchar(a)), "left")

  # glue::glue("  {fr(names)}  {fr(types)}  {fr(tags)}")
