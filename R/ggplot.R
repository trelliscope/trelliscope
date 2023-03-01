#' @export
print.facet_panels <- function(x, ...) {
  nm <- x$labels$title
  if (is.null(nm))
    nm <- "ggplot"
  dsc <- paste(c("Faceted by ", attr(x, "trelliscope")$facets), collapse = "")
  x |>
    nest_panels() |>
    as_trelliscope(name = nm, description = dsc, path = tempfile()) |>
    write_trelliscope() |>
    view_trelliscope()
}
