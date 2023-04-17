#' @importFrom knitr opts_knit opts_current
in_rmarkdown <- function() {
  tmp <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  !is.null(tmp) && tmp == "html"
}

# # TODO: warn if self-contained is TRUE and don't print
# can_print_rmarkdown <- function() {
#   knitr::opts_knit$get("self.contained")
# }

#' @importFrom glue glue
knit_print.trelliscope <- function(x, ...) {
  trobj <- attr(x, "trelliscope")
  cur_opts <- knitr::opts_current$get()
  url <- paste0(trobj$path, "/index.html")
  # TODO: enforce minimum width
  width <- cur_opts$out.width.px
  height <- cur_opts$out.height.px
  title <- trobj$get("name")
  iframe <- glue::glue("
    <iframe
      src=\"{url}\"
      title=\"{title}\"
      width=\"{width}px\"
      height=\"{height}px\"
      allowfullscreen
      style=\"margin: 0; padding: 0; border: none;\"
    >
    </iframe>
  ")
  knitr::asis_output(iframe)
}

registerS3method(
  "knit_print", "trelliscope", knit_print.trelliscope,
  envir = asNamespace("knitr")
)
