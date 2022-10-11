dat <- ggplot2::mpg |>
  tidyr::nest(data = !dplyr::one_of(c("manufacturer", "class"))) |>
  dplyr::mutate(panel = map_plot(data, function(x) {
    ggplot2::qplot(hwy, cty, data = x)
  }))

x <- trelliscope(dat, name = "test")

test_that("add_meta_def", {
  expect_error(
    b <- x |>
      add_meta_def(meta_string("manufacturr", "vehicle manufacturer")),
    regexp = "not find variable"
  )

  b <- x |>
    add_meta_def(meta_string("manufacturer", "vehicle manufacturer"))
  expect_true("manufacturer" %in% names(b$get("metas")))

  # make sure original object's underlying R6 class was cloned
  expect_length(x$get("metas"), 0)

  expect_message(
    b2 <- b |>
      add_meta_def(meta_string("manufacturer", "vehicle manufacturer")),
    regexp = "Replacing existing meta variable definition"
  )
})

test_that("add_meta_defs", {
  b <- x |>
    add_meta_defs(
      meta_string("manufacturer", "vehicle manufacturer")
    )
  expect_length(b$get("metas"), 1)

  b <- x |>
    add_meta_defs(
      meta_string("manufacturer", "vehicle manufacturer"),
      meta_string("class", "vehicle class")
    )
  expect_length(b$get("metas"), 2)
  expect_true(all(c("manufacturer", "class") %in% names(b$get("metas"))))

  expect_error(
    b <- x |>
      add_meta_defs(meta_string("manufacturr", "vehicle manufacturer")),
    regexp = "not find variable"
  )

  # make sure original object's underlying R6 class was cloned
  expect_length(x$get("metas"), 0)

  expect_message(
    b2 <- b |>
      add_meta_defs(meta_string("manufacturer", "vehicle manufacturer")),
    regexp = "Replacing existing meta variable definition"
  )
})

test_that("set_layout", {
  b <- x |>
    set_layout()
  obj <- b$get("state")$get("layout")
  expect_true(!is.null(obj))
  expect_equal(obj$get("type"), "layout")

  # make sure we haven't changed the underlying object
  expect_null(x$get("state")$get("layout"))

  expect_message(
    a <- b |>
      set_layout(nrow = 3, ncol = 3),
    regexp = "Replacing existing layout state specification"
  )

  expect_equal(a$get("state")$get("layout")$get("nrow"), 3)

  # make sure "b" wasn't changed
  expect_equal(b$get("state")$get("layout")$get("nrow"), 1)
})

test_that("set_labels", {
  b <- x |>
    set_labels(varnames = "manufacturer")

  obj <- b$get("state")$get("labels")
  expect_true(!is.null(obj))
  expect_equal(obj$get("type"), "labels")
  expect_equal(obj$get("varnames"), I("manufacturer"))

  # make sure we haven't changed the underlying object
  expect_null(x$get("state")$get("labels"))

  expect_message(
    a <- b |>
      set_labels(varnames = NULL),
    "Replacing existing labels state specification"
  )

  expect_length(a$get("state")$get("labels")$get("varnames"), 0)

  # make sure "b" hasn't been changed
  expect_equal(b$get("state")$get("labels")$get("varnames"), I("manufacturer"))
})

test_that("set_sort", {
  expect_error(
    b <- x |>
      set_sort(varnames = c("a", "b"), dirs = "asc"),
    regexp = "must have same length"
  )

  b <- x |>
    set_sort(varnames = c("manufacturer", "class"), dirs = c("asc", "desc"))

  obj <- b$get("state")$get("sort")
  expect_length(obj, 2)
  expect_equal(obj[[1]]$get("type"), "sort")

  # make sure we haven't changed the underlying object
  expect_length(x$get("state")$get("sort"), 0)

  expect_message(
    a <- b |>
      set_sort(varnames = "manufacturer", dirs = "asc", add = TRUE),
    regexp = "Replacing existing sort state specification for variable"
  )

  # "class" should now be first
  expect_equal(names(a$get("state")$get("sort"))[1], "class")

  expect_message(
    b |>
      set_sort(varnames = "manufacturer", dirs = "desc"),
    regexp = "Replacing entire existing sort"
  )
})
