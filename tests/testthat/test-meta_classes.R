dat <- iris
sq <- seq_len(nrow(dat))
dat$id <- as.character(sq)
dat$letters <- rep(letters, 10)[1:150]
dat$date <- Sys.Date() + 1:150
dat$date2 <- as.character(dat$date)
dat$datetime <- as.POSIXct(dat$date)
tmp <- matrix(c(
  c(2:150, 1),
  c(3:150, 1:2)
), ncol = 2)
dat$lst <- apply(tmp, 1, as.list)
dat$lat <- runif(150, -90, 90)
dat$long <- runif(150, 0, 180)
dat$href <- "https://google.com"

test_that2("NumberMeta", {
  obj <- NumberMeta$new("Sepal.Length", tags = "stuff")
  expect_true(
    obj$check_with_data(dat)
  )

  expect_equal(obj$get("tags"), I("stuff"))

  expect_equal(
    as.character(obj$as_json(pretty = FALSE)),
    '{"locale":true,"digits":null,"sortable":true,"filterable":true,"tags":["stuff"],"label":"Sepal.Length","type":"number","varname":"Sepal.Length"}'
  )

  expect_true(
    is.list(obj$as_list())
  )

  obj <- NumberMeta$new("Sepal.Length",
    label = "Sepal length of the iris")

  obj <- NumberMeta$new("whatever", digits = 2, local = FALSE)
  expect_error(
    obj$check_with_data(dat),
    regexp = "Could not find variable"
  )

  obj <- NumberMeta$new("Species")
  expect_error(
    obj$check_with_data(dat),
    regexp = "must be numeric"
  )

  expect_error(
    NumberMeta$new("Sepal.Length", digits = "a"),
    regexp = "must be an integer"
  )

  expect_error(
    NumberMeta$new("Sepal.Length", locale = "a"),
    regexp = "must be logical"
  )
})

test_that2("CurrencyMeta", {
  obj <- CurrencyMeta$new("Sepal.Length", tags = "stuff")
  expect_true(
    obj$check_with_data(dat)
  )

  expect_equal(obj$get("tags"), I("stuff"))

  expect_equal(
    as.character(obj$as_json(pretty = FALSE)),
    '{"code":"USD","sortable":true,"filterable":true,"tags":["stuff"],"label":"Sepal.Length","type":"currency","varname":"Sepal.Length"}'
  )

  expect_true(
    is.list(obj$as_list())
  )

  obj <- CurrencyMeta$new("Sepal.Length",
    label = "Sepal length of the iris")

  obj <- CurrencyMeta$new("whatever")
  expect_error(
    obj$check_with_data(dat),
    regexp = "Could not find variable"
  )

  obj <- CurrencyMeta$new("Species")
  expect_error(
    obj$check_with_data(dat),
    regexp = "must be numeric"
  )

  expect_error(
    CurrencyMeta$new("Sepal.Length", code = "ASD"),
    regexp = "must be one of"
  )
})

test_that2("StringMeta", {
  obj <- StringMeta$new("Species")
  expect_true(
    obj$check_with_data(dat)
  )

  expect_true(
    all(obj$cast_variable(dat)$Species == as.character(dat$Species))
  )

  obj <- StringMeta$new("Sepal.Length")
  expect_true(
    obj$check_with_data(dat)
  )

  expect_true(
    all(obj$cast_variable(dat)$Sepal.Length == as.character(dat$Sepal.Length))
  )

  obj <- StringMeta$new("lst")
  expect_error(
    obj$check_with_data(dat),
    regexp = "must be an atomic vector"
  )
})

test_that2("FactorMeta", {
  obj <- FactorMeta$new("Species",
    levels = c("setosa", "versicolor", "virginica"))
  expect_true(
    obj$check_with_data(dat)
  )

  expect_true(
    all(obj$cast_variable(dat)$Species == as.character(dat$Species))
  )

  obj <- FactorMeta$new("Species",
    levels = c("setosa", "versicolor"))
  expect_error(
    obj$check_with_data(dat),
    regexp = "contains values not specified"
  )

  obj <- FactorMeta$new("Species",
    levels = c("setosa", "versicolor", "virginica", "stuff"))
  expect_true(
    obj$check_with_data(dat)
  )
})

test_that2("DateMeta", {
  obj <- DateMeta$new("date")
  expect_true(
    obj$check_with_data(dat)
  )

  expect_true(
    all(obj$cast_variable(dat)$date == dat$date)
  )

  obj2 <- DateMeta$new("date2")
  expect_true(
    all(obj2$cast_variable(dat)$date2 == dat$date)
  )

  obj <- DateMeta$new("lst")
  expect_error(
    obj$check_with_data(dat),
    regexp = "must have class 'Date'"
  )
})

test_that2("DatetimeMeta", {
  obj <- DatetimeMeta$new("datetime")
  expect_true(
    obj$check_with_data(dat)
  )

  expect_true(
    all(obj$cast_variable(dat)$datetime == dat$datetime)
  )

  obj <- DatetimeMeta$new("lst")
  expect_error(
    obj$check_with_data(dat),
    regexp = "must have class 'POSIXct'"
  )
})

test_that2("GeoMeta", {
  obj <- GeoMeta$new("coords", latvar = "lat", longvar = "long")
  expect_true(
    obj$check_with_data(dat)
  )

  tmp <- obj$cast_variable(dat)
  expect_true(
    !any(c("lat", "long") %in% names(tmp))
  )
  expect_true(
    "coords" %in% names(tmp)
  )
  expect_true(
    tmp$coords[[1]][[1]] == dat$lat[1]
  )
})

test_that2("GraphMeta", {
  obj <- GraphMeta$new("lst", idvarname = "id", direction = "to")
  expect_true(
    obj$check_with_data(dat)
  )

  expect_error(
    obj <- GraphMeta$new("lst", idvarname = "id", direction = "stuff"),
    regexp = "must be one of"
  )

  obj2 <- GraphMeta$new("lst", idvarname = "stuff")
  expect_error(
    obj2$check_with_data(dat),
    regexp = "not find variable"
  )
})

test_that2("HrefMeta", {
  obj <- HrefMeta$new("href")
  expect_true(
    obj$check_with_data(dat)
  )

  expect_true(
    all(obj$cast_variable(dat)$href == as.character(dat$href))
  )

  obj <- HrefMeta$new("lst")
  expect_error(
    obj$check_with_data(dat),
    regexp = "must be an atomic vector"
  )
})
