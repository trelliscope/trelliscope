dat <- ggplot2::mpg %>%
  tidyr::nest(data = !dplyr::one_of(c("manufacturer", "class"))) %>%
  dplyr::mutate(panel = map_plot(data, function(x) {
    ggplot2::qplot(hwy, cty, data = x)
  }))

x <- trelliscope(dat, name = "test")

test_that("add_meta_def", {
  expect_error(
    b <- x %>%
      add_meta_def(meta_string("manufacturr", "vehicle manufacturer")),
    regexp = "not find variable"
  )

  b <- x %>%
    add_meta_def(meta_string("manufacturer", "vehicle manufacturer"))
  expect_true("manufacturer" %in% names(b$get("metas")))

  # make sure original object's underlying R6 class was cloned
  expect_length(x$get("metas"), 0)

  expect_message(
    b2 <- b %>%
      add_meta_def(meta_string("manufacturer", "vehicle manufacturer")),
    regexp = "Replacing existing meta variable definition"
  )
})

test_that("add_meta_defs", {
  b <- x %>%
    add_meta_defs(
      meta_string("manufacturer", "vehicle manufacturer")
    )
  expect_length(b$get("metas"), 1)

  b <- x %>%
    add_meta_defs(
      meta_string("manufacturer", "vehicle manufacturer"),
      meta_string("class", "vehicle class")
    )
  expect_length(b$get("metas"), 2)
  expect_true(all(c("manufacturer", "class") %in% names(b$get("metas"))))

  expect_error(
    b <- x %>%
      add_meta_defs(meta_string("manufacturr", "vehicle manufacturer")),
    regexp = "not find variable"
  )

  # make sure original object's underlying R6 class was cloned
  expect_length(x$get("metas"), 0)

  expect_message(
    b2 <- b %>%
      add_meta_defs(meta_string("manufacturer", "vehicle manufacturer")),
    regexp = "Replacing existing meta variable definition"
  )
})

test_that("set_layout", {
  b <- x %>%
    set_layout()
  obj <- b$get("state")$get("layout")
  expect_true(!is.null(obj))
  expect_equal(obj$get("type"), "layout")

  # make sure we haven't changed the underlying object
  expect_null(x$get("state")$get("layout"))
})

test_that("set_labels", {
  b <- x %>%
    set_labels(varnames = "manufacturer")

  obj <- b$get("state")$get("labels")
  expect_true(!is.null(obj))
  expect_equal(obj$get("type"), "labels")
  expect_equal(obj$get("varnames"), I("manufacturer"))

  # make sure we haven't changed the underlying object
  expect_null(x$get("state")$get("labels"))
})

test_that("set_sort", {
  expect_error(
    b <- x %>%
      set_sort(varnames = c("a", "b"), dirs = "asc"),
    regexp = "must have same length"
  )

  b <- x %>%
    set_sort(varnames = c("manufacturer", "class"), dirs = c("asc", "desc"))

  obj <- b$get("state")$get("sort")
  expect_length(obj, 2)
  expect_equal(obj[[1]]$get("type"), "sort")

  # make sure we haven't changed the underlying object
  expect_length(x$get("state")$get("sort"), 0)

  expect_message(
    b %>%
      set_sort(varnames = "manufacturer", dirs = "desc"),
    regexp = "Replacing entire existing sort"
  )
})
