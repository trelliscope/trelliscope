dat <- iris
sq <- seq_len(nrow(dat))
dat$id <- as.character(sq)
dat$letters <- rep(letters, 10)[1:150]
dat$date <- Sys.Date() + 1:150
dat$datetime <- as.POSIXct(dat$date)
tmp <- matrix(c(
  c(2:150, 1),
  c(3:150, 1:2)
), ncol = 2)
dat$lst <- apply(tmp, 1, as.list)

test_that("LayoutState", {
  obj <- LayoutState$new()
  expect_true(obj$check_with_data(dat))

  expect_error(
    LayoutState$new(nrow = "a"),
    regexp = "must be an integer"
  )

  expect_error(
    LayoutState$new(arrange = "stuff"),
    regexp = "must be one of rows, cols"
  )
})

test_that("LabelState", {
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

test_that("SortState", {
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
    regexp = "'dir' must be one of"
  )
})
