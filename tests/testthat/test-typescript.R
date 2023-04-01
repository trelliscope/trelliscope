case_to_string <- function(cls) {
  obj <- cls$as_list()
  obj <- obj[sort(names(obj))]
  obj |> to_json() |> as.character()
}
path <- system.file("types", package = "trelliscope")
read_case <- function(obj) {
  name <- deparse(substitute(obj))
  readLines(paste0(path, "/json/", name, ".json"), warn = FALSE)
}

# -------------------------------------------------------- #
# meta                                                     #
# -------------------------------------------------------- #
test_that2("typescript meta comparison", {
  meta_num_min <- NumberMeta$new(
    varname = "numvar",
    digits = 2,
    log = FALSE,
  ) |> case_to_string()
  ts <- read_case(meta_num_min)
  expect_equal(meta_num_min, ts)

  meta_num_full <- NumberMeta$new(
    varname = "numvar",
    label = "numvar label",
    tags = c("a", "b", "c"),
    digits = 2,
    log = FALSE,
    locale = FALSE
  ) |> case_to_string()
  ts <- read_case(meta_num_full)
  expect_equal(meta_num_full, ts)

  meta_string <- StringMeta$new(varname = "stringvar") |>
    case_to_string()
  ts <- read_case(meta_string)
  expect_equal(meta_string, ts)

  meta_fac <- FactorMeta$new(
    varname = "facvar",
    levels = c("l1", "l2", "l3")
  ) |> case_to_string()
  ts <- read_case(meta_fac)
  expect_equal(meta_fac, ts)

  meta_dt <- DateMeta$new(varname = "datevar") |> case_to_string()
  ts <- read_case(meta_dt)
  expect_equal(meta_dt, ts)

  meta_dttm <- DatetimeMeta$new(varname = "datetimevar") |> case_to_string()
  ts <- read_case(meta_dttm)
  expect_equal(meta_dttm, ts)

  meta_href <- HrefMeta$new(varname = "hrefvar") |> case_to_string()
  ts <- read_case(meta_href)
  expect_equal(meta_href, ts)

  meta_geo <- GeoMeta$new(
    varname = "geovar",
    latvar = "var1",
    longvar = "var2"
  ) |> case_to_string()
  ts <- read_case(meta_geo)
  expect_equal(meta_geo, ts)

  meta_grph <- GraphMeta$new(varname = "graphvar", idvarname = "idvar",
    linkidvarname = "linkidvar", labelvarname = "idvar") |>
    case_to_string()
  ts <- read_case(meta_grph)
  expect_equal(meta_grph, ts)
})

# -------------------------------------------------------- #
# inputs                                                   #
# -------------------------------------------------------- #
test_that2("typescript inputs comparison", {
  options <- c("a", "b", "c")

  input_radio <- RadioInput$new(name = "radio", options = options) |>
    case_to_string()
  ts <- read_case(input_radio)
  expect_equal(input_radio, ts)

  input_check <- CheckboxInput$new(name = "check", options = options) |>
    case_to_string()
  ts <- read_case(input_check)
  expect_equal(input_check, ts)

  input_select <- SelectInput$new(name = "select", options = options) |>
    case_to_string()
  ts <- read_case(input_select)
  expect_equal(input_select, ts)

  input_mselect <- MultiselectInput$new(name = "mselect", options = options) |>
    case_to_string()
  ts <- read_case(input_mselect)
  expect_equal(input_mselect, ts)

  input_text <- TextInput$new(name = "text") |>
    case_to_string()
  ts <- read_case(input_text)
  expect_equal(input_text, ts)

  input_num <- NumberInput$new(name = "num") |>
    case_to_string()
  ts <- read_case(input_num)
  expect_equal(input_num, ts)
})

# -------------------------------------------------------- #
# states                                                   #
# -------------------------------------------------------- #
test_that2("typescript states comparison", {
  state_layout <- LayoutState$new() |>
    case_to_string()
  ts <- read_case(state_layout)
  expect_equal(state_layout, ts)

  state_label <- LabelState$new(varnames = c("a", "b")) |>
    case_to_string()
  ts <- read_case(state_label)
  expect_equal(state_label, ts)

  state_sort <- SortState$new(varname = "a")
  state_sort$set("metatype", "string")
  ts <- read_case(state_sort)
  expect_equal(case_to_string(state_sort), ts)

  state_catfilt <- CategoryFilterState$new(
    varname = "a", values = c("a", "b", "c"))
  state_catfilt$set("metatype", "string")
  ts <- read_case(state_catfilt)
  expect_equal(case_to_string(state_catfilt), ts)

  state_numfilt <- NumberRangeFilterState$new(varname = "a", min = 1) |>
    case_to_string()
  ts <- read_case(state_numfilt)
  expect_equal(state_numfilt, ts)

  # # need to test these later...
  # state_dtfilt <- DateRangeFilterState$new(
  #   varname = "a", min = as.Date("2000-01-01")) |>
  #   case_to_string()
  # ts <- read_case(state_dtfilt)
  # expect_equal(state_dtfilt, ts)

  # state_dttmfilt <- DatetimeRangeFilterState$new(
  #   varname = "a", min = as.POSIXct(as.Date("2000-01-01"))) |>
  #   case_to_string()
  # ts <- read_case(state_dttmfilt)
  # expect_equal(state_dttmfilt, ts)

  # displst_min <- DisplayState$new()
  # displst_min$set(LabelState$new(varnames = c("a", "b")))
  # displst_min <- case_to_string(displst_min)
  # ts <- read_case(displst_min)
  # expect_equal(displst_min, ts)

  # displst <- DisplayState$new()
  # displst$set(LabelState$new(varnames = c("a", "b")))
  # displst$set(LayoutState$new(ncol = 4))
  #   sort: [
  #     new SortState({ varname = "a" ),
  #     new SortState({ varname = "b", dir: "desc" )
  #   ],
  #   filter: [
  #     new CategoryFilterState({ varname = "a", values = c("a", "b", "c")),
  #     new NumberRangeFilterState({ varname = "a", min = 1 )
  #   ]
  # })
  # ts <- read_case(displst)
  # expect_equal(displst, ts)
})

# /* ------------------------------------------------------ */
# /* view                                                   */
# /* ------------------------------------------------------ */

# const view = new View({
#   name: 'myview',
#   state: displst_min
# })
# writeCase(view, 'view')

# const view2 = new View({
#   name: 'myview2',
#   state: displst
# })

# /* ------------------------------------------------------ */
# /* display                                                */
# /* ------------------------------------------------------ */

# const displ = new Display({
#   name: 'test display',
#   keycols: ['a', 'b'],
#   metas: [
#     new StringMeta({ varname = 'stringvar' }),
#     new DateMeta({ varname = 'datevar' })
#   ],
#   inputs: [
#     new SelectInput({ name: 'select', options })
#   ],
#   state: displst,
#   views: [
#     view,
#     view2
#   ]
# })
# writeCase(displ, 'displ')
