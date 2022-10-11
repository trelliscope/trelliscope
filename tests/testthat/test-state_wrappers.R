# TODO: move all this testing to the class tests and just make sure
# the wrappers run without error (all error handling is in classes)
test_that("state_layout", {
  obj <- state_layout(nrow = 2, ncol = 3, arrange = "cols", page = 1)

  expect_true(inherits(obj, "trelliscope_state_def"))
  expect_equal(obj$get("type"), "layout")

  expect_error(
    state_layout(nrow = "a"),
    regexp = "must be an integer"
  )
})

test_that("state_labels", {
  obj <- state_labels(varnames = c("a", "b", "c"))

  expect_true(inherits(obj, "trelliscope_state_def"))
  expect_equal(obj$get("type"), "labels")

  expect_error(
    obj$check_with_data(iris),
    regexp = "Label variables not found"
  )

  obj <- state_labels(varnames = "Sepal.Length")
  expect_true(obj$check_with_data(iris))

  expect_error(
    state_labels(varnames = 1),
    regexp = "must be of type 'character'"
  )
})

test_that("state_sort", {
  obj <- state_sort(varname = "a", dir = "desc")

  expect_true(inherits(obj, "trelliscope_state_def"))
  expect_equal(obj$get("type"), "sort")

  expect_error(
    obj$check_with_data(iris),
    regexp = "not found in the dataset"
  )

  expect_error(
    state_sort(varname = "a", dir = "descc"),
    regexp = "must be one of asc, desc"
  )

  expect_error(
    state_sort(varname = c("a", "b"), dir = "descc"),
    regexp = "must be a scalar value"
  )

  obj <- state_sort(varname = "Sepal.Length")
  expect_true(obj$check_with_data(iris))
})
