dat <- iris
sq <- seq_len(nrow(dat))
dat$id <- as.character(sq)
dat$letters <- rep(letters, 10)[1:150]
dat$date <- as.Date("2000-01-01") + 1:150
dat$datetime <- as.POSIXct(dat$date)
dat$datestring <- as.character(dat$date)
tmp <- matrix(c(
  c(2:150, 1),
  c(3:150, 1:2)
), ncol = 2)
dat$lst <- apply(tmp, 1, as.list)

test_that2("LayoutState", {
  obj <- LayoutState$new()
  expect_true(obj$check_with_data(dat))

  expect_error(
    LayoutState$new(nrow = "a"),
    regexp = "must be an integer"
  )

  expect_error(
    LayoutState$new(arrange = "stuff"),
    regexp = "must be one of \"rows\""
  )
})

test_that2("LabelState", {
  obj <- LabelState$new(c("Species", "date"))
  expect_true(obj$check_with_data(dat))

  expect_error(
    LabelState$new(1),
    regexp = "must be of type"
  )

  obj <- LabelState$new(c("Species", "date", "stuff"))
  expect_error(
    obj$check_with_data(dat),
    regexp = "variables not found"
  )
})

test_that2("SortState", {
  obj <- SortState$new("date")
  obj_meta <- DateMeta$new("date")

  expect_true(obj$check_with_data(dat))
  expect_true(obj$check_with_meta(obj_meta))

  expect_equal(
    obj$as_list(),
    list(dir = "asc", varname = "date", type = "sort")
  )

  expect_equal(
    as.character(obj$as_json()),
    '{"dir":"asc","varname":"date","type":"sort"}'
  )

  expect_error(
    SortState$new(1),
    regexp = "must be of type"
  )

  expect_error(
    SortState$new(c("var1", "var2")),
    regexp = "must be a scalar"
  )

  obj <- SortState$new("stuff")
  expect_error(
    obj$check_with_data(dat),
    regexp = "not found in the dataset"
  )

  expect_error(
    SortState$new("date", dir = "ascc"),
    regexp = "\"dir\" must be one of"
  )
})

test_that2("CategoryFilterState", {
  obj <- CategoryFilterState$new("datestring", values = "2000-01-02")
  obj_meta1 <- StringMeta$new("datestring")
  obj_meta2 <- FactorMeta$new("datestring")
  obj_meta3 <- DateMeta$new("date")

  expect_true(obj$check_with_data(dat))
  expect_true(obj$check_with_meta(obj_meta1))
  expect_true(obj$check_with_meta(obj_meta2))
  expect_error(
    obj$check_with_meta(obj_meta3),
    regexp = "is not compatible with this filter"
  )

  expect_equal(
    obj$as_list(),
    list(values = I("2000-01-02"), regexp = NULL, filtertype = "category",
      varname = "datestring", type = "filter")
  )

  expect_equal(
    as.character(obj$as_json()),
    '{"values":["2000-01-02"],"regexp":null,"filtertype":"category","varname":"datestring","type":"filter"}'
  )

  obj <- CategoryFilterState$new("datestring", values = "stuff")

  expect_error(
    obj$check_with_data(dat),
    regexp = "could not find the value"
  )

  expect_error(
    CategoryFilterState$new("date", values = 1),
    regexp = "must be of type 'character'"
  )
})

test_that2("NumberRangeFilterState", {
  obj <- NumberRangeFilterState$new("Sepal.Length", min = 1)
  obj_meta1 <- NumberMeta$new("Sepal.Length")
  obj_meta2 <- StringMeta$new("Species")

  expect_true(obj$check_with_data(dat))
  expect_true(obj$check_with_meta(obj_meta1))
  expect_error(
    obj$check_with_meta(obj_meta2),
    regexp = "is not compatible with this filter"
  )

  expect_equal(
    obj$as_list(),
    list(max = NULL, min = 1, filtertype = "numberrange",
      varname = "Sepal.Length", type = "filter")
  )

  expect_equal(
    as.character(obj$as_json()),
    '{"max":null,"min":1,"filtertype":"numberrange","varname":"Sepal.Length","type":"filter"}'
  )

  obj <- NumberRangeFilterState$new("stuff", min = 1)
  expect_error(
    obj$check_with_data(dat),
    regexp = "not found in the dataset"
  )

  expect_error(
    NumberRangeFilterState$new("stuff", min = "a"),
    regexp = "\"min\" must be numeric"
  )

  expect_error(
    NumberRangeFilterState$new("stuff", max = "a"),
    regexp = "\"max\" must be numeric"
  )
})

test_that2("DateRangeFilterState", {
  obj <- DateRangeFilterState$new("date", min = as.Date("2010-01-01"))
  obj_meta1 <- DateMeta$new("date")
  obj_meta2 <- StringMeta$new("Species")

  expect_true(obj$check_with_data(dat))
  expect_true(obj$check_with_meta(obj_meta1))
  expect_error(
    obj$check_with_meta(obj_meta2),
    regexp = "is not compatible with this filter"
  )

  expect_equal(
    obj$as_list(),
    list(max = NULL, min = structure(14610, class = "Date"),
      filtertype = "daterange", varname = "date", type = "filter")
  )

  expect_equal(
    as.character(obj$as_json()),
    '{"max":null,"min":"2010-01-01","filtertype":"daterange","varname":"date","type":"filter"}'
  )

  obj <- DateRangeFilterState$new("stuff", min = as.Date("2010-01-01"))
  expect_error(
    obj$check_with_data(dat),
    regexp = "not found in the dataset"
  )

  expect_error(
    DateRangeFilterState$new("stuff", min = "a"),
    regexp = "\"min\" must have class 'Date'"
  )

  expect_error(
    DateRangeFilterState$new("stuff", max = "a"),
    regexp = "\"max\" must have class 'Date'"
  )
})

test_that2("DatetimeRangeFilterState", {
  obj <- DatetimeRangeFilterState$new("datetime",
    min = as.POSIXct("2010-01-01"))
  obj_meta1 <- DatetimeMeta$new("datetime")
  obj_meta2 <- StringMeta$new("Species")

  expect_true(obj$check_with_data(dat))
  expect_true(obj$check_with_meta(obj_meta1))
  expect_error(
    obj$check_with_meta(obj_meta2),
    regexp = "is not compatible with this filter"
  )

  # expect_equal(
  #   obj$as_list(),
  #   list(max = NULL, min = structure(1262332800, class = c("POSIXct",
  #     "POSIXt"), tzone = ""), filtertype = "datetimerange",
  #     varname = "datetime",
  #   type = "filter")
  # )

  # expect_equal(
  #   as.character(obj$as_json()),
  #   '{"max":null,"min":"2010-01-01","filtertype":"datetimerange","varname":"datetime","type":"filter"}'
  # )

  obj <- DatetimeRangeFilterState$new("stuff", min = as.POSIXct("2010-01-01"))
  expect_error(
    obj$check_with_data(dat),
    regexp = "not found in the dataset"
  )

  expect_error(
    DatetimeRangeFilterState$new("stuff", min = "a"),
    regexp = "\"min\" must have class 'POSIXct'"
  )

  expect_error(
    DatetimeRangeFilterState$new("stuff", max = "a"),
    regexp = "\"max\" must have class 'POSIXct'"
  )
})
