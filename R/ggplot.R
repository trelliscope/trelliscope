#' @export
print.facet_panels <- function(x, ..., view = TRUE) {
  nm <- x$labels$title
  if (is.null(nm))
    nm <- "ggplot"
  dsc <- paste(c("Faceted by ", attr(x, "trelliscope")$facets), collapse = "")
  res <- x |>
    as_panels_df() |>
    as_trelliscope_df(name = nm, description = dsc, path = tempfile()) |>
    write_trelliscope()

  if (interactive() && view)
    view_trelliscope(res)

  invisible(res)
}
