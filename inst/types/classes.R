list_diff <- function(a, b) {
  nms <- names(b)
  a[setdiff(names(a), nms)]
}

meta <- Meta$new(
  varname = "test",
  label = "test label",
  tags = c("a", "b", "c"),
  type = "number",
  sortable = FALSE,
  filterable = FALSE
)

as_json(meta)
# {
#   "sortable": false,
#   "filterable": false,
#   "tags": ["a", "b", "c"],
#   "label": "test label",
#   "type": "number",
#   "varname": "test"
# }

num <- meta_number(
  "test",
  label = "test label",
  tags = c("a", "b", "c"),
  digits = 2L,
  locale = TRUE
)

list_diff(num$as_list(), meta$as_list()) |>
  to_json(pretty = TRUE)
# {
#   "locale": true,
#   "digits": 2
# }

fac <- meta_factor(
  "test",
  label = "test label",
  tags = c("a", "b", "c"),
  levels = c("asdf", "qwerty")
)
list_diff(fac$as_list(), meta$as_list()) |>
  to_json(pretty = TRUE)
# {
#   "levels": ["asdf", "qwerty"]
# }

chr <- meta_string(
  "test",
  label = "test label",
  tags = c("a", "b", "c")
)
list_diff(chr$as_list(), meta$as_list()) |>
  to_json(pretty = TRUE)
# {}

dte <- meta_date(
  "test",
  label = "test label",
  tags = c("a", "b", "c")
)
list_diff(dte$as_list(), meta$as_list()) |>
  to_json(pretty = TRUE)
# {}

dtt <- meta_datetime(
  "test",
  label = "test label",
  tags = c("a", "b", "c")
)
list_diff(dtt$as_list(), meta$as_list()) |>
  to_json(pretty = TRUE)
# {
#   "timezone": "UTC"
# }

hrf <- meta_href(
  "test",
  label = "test label",
  tags = c("a", "b", "c")
)
list_diff(hrf$as_list(), meta$as_list()) |>
  to_json(pretty = TRUE)
# {}

geo <- meta_geo(
  "test",
  latvar = "lat",
  longvar = "long",
  label = "test label",
  tags = c("a", "b", "c")
)
list_diff(geo$as_list(), meta$as_list()) |>
  to_json(pretty = TRUE)
# {}

grp <- meta_graph(
  "test",
  label = "test label",
  tags = c("a", "b", "c"),
  idvarname = "idvar",
  direction = "none"
)
list_diff(grp$as_list(), meta$as_list()) |>
  to_json(pretty = TRUE)
# {
#   "direction": "none",
#   "idvarname": "idvar"
# }



# library(V8)
# ct <- v8()
# ct$source("_ignore/types/meta_types2.js")
# ct$get(JS('Object.keys(global)'))
# ct$get(JS("typeof NumberMeta"))
# ct$get(JS("new NumberMeta('test')"))
# ct$get(JS("Meta"))
# ct$call("Meta")
# ct$call("print", "hi")
