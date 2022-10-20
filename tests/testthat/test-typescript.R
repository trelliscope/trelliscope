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

test_that("typescript comparison", {
  meta_num_min <- NumberMeta$new(
    varname = "numvar"
  ) |> case_to_string()
  ts <- read_case(meta_num_min)
  expect_equal(meta_num_min, ts)

  meta_num_full <- NumberMeta$new(
    varname = "numvar",
    label = "numvar label",
    tags = c("a", "b", "c"),
    digits = 2,
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

  meta_grph <- GraphMeta$new(varname = "graphvar", idvarname = "idvar") |>
    case_to_string()
  ts <- read_case(meta_grph)
  expect_equal(meta_grph, ts)
})
