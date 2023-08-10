#' @importFrom knitr opts_knit opts_current
in_rmarkdown <- function() {
  tmp <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  !is.null(tmp)
}

can_print_rmarkdown <- function() {
  pandoc_to <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  pandoc_args <- knitr::opts_knit$get("rmarkdown.pandoc.args")
  valid <- c("html", "revealjs", "slidy")
  pandoc_to %in% valid && !any(pandoc_args == "--self-contained")
}

# #' @importFrom glue glue
# knit_print.trelliscope <- function(x, ...) {
#   trobj <- attr(x, "trelliscope")
#   cur_opts <- knitr::opts_current$get()
#   url <- paste0(trobj$path, "/index.html")
#   # TODO: enforce minimum width
#   width <- cur_opts$out.width.px
#   height <- cur_opts$out.height.px
#   title <- trobj$get("name")
#   iframe <- glue::glue("
#     <iframe
#       src=\"{url}\"
#       title=\"{title}\"
#       width=\"{width}px\"
#       height=\"{height}px\"
#       allowfullscreen
#       style=\"margin: 0; padding: 0; border: 1px solid #efefef;\"
#     >
#     </iframe>
#   ")
#   knitr::asis_output(iframe)
# }

# registerS3method(
#   "knit_print", "trelliscope", knit_print.trelliscope,
#   envir = asNamespace("knitr")
# )

# nolint start

#' Shiny bindings for trelliscope
#'
#' Output and render functions for using trelliscope within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param expr An expression that generates a trelliscope
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#' @param ... Argumentsed passed on to [shiny::htmlOutput()]
#'   is useful if you want to save an expression in a variable.
#'
#' @name trelliscope-shiny
#'
#' @importFrom shiny htmlOutput
#' @export
trelliscopeOutput <- function(outputId, ...) {
  shiny::htmlOutput(outputId = outputId, inline = FALSE, ...)
}

#' @importFrom shiny createRenderFunction installExprFunction
#' @importFrom htmltools tags doRenderTags
#' @importFrom digest digest
#' @rdname trelliscope-shiny
#' @export
renderTrelliscope <- function(expr, env = parent.frame(), quoted = FALSE) {
  func <- shiny::installExprFunction(expr, "func", env, quoted)

  shiny::createRenderFunction(
    func,
    transform = function(value, session, name, ...) {
      if (inherits(value, "trelliscope")) {
        trobj <- attr(value, "trelliscope")
        prefix <- getOption(".trelliscope_shiny_resource_prefix")
        if (is.null(prefix)) {
          src <- paste0("trelliscope/", name, "/index.html")
        } else {
          prefix <- getOption(".trelliscope_shiny_resource_prefix")
          pth <- getOption(".trelliscope_shiny_resource_path")
          tmp <- gsub(pth, prefix, trobj$path)
          src <- paste0(tmp, "/index.html")
        }

        write_trelliscope(value)
        html <- htmltools::tags$iframe(
          src = src,
          class = digest::digest(trobj),
          title = name,
          width = "100%",
          height = "100%",
          allowfullscreen = TRUE,
          style = "margin: 0; padding: 0; border: 1px solid #efefef;"
        )
      } else {
        html <- htmltools::tags$div("[No trelliscope display]")
      }
      htmltools::doRenderTags(list(html = html))
    },
    outputFunc = trelliscopeOutput
  )
}
# nolint end

#' Add Trelliscope resource path for Shiny app
#' @param prefix,path See [shiny::addResourcePath()]
#' @export
#' @importFrom shiny addResourcePath
add_trelliscope_resource_path <- function(prefix, path) {
  shiny::addResourcePath(prefix, path)
  options(".trelliscope_shiny_resource_prefix" = paste0("/", prefix))
  options(".trelliscope_shiny_resource_path" = path)
  invisible(NULL)
}
