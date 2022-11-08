dat <- ggplot2::mpg |>
  tidyr::nest(data = !dplyr::one_of(c("manufacturer", "class"))) |>
  dplyr::mutate(panel = map_plot(data, function(x) {
    ggplot2::qplot(hwy, cty, data = x)
  }))

test_that("trelliscope instantiation", {
  expect_error(
    trelliscope(iris, name = "test"),
    regexp = "that references a plot or image"
  )

  suppressMessages(expect_error(
    trelliscope(dat),
    regexp = "argument \"name\" is missing"
  ))

  expect_message(
    x <- trelliscope(dat, name = "test"),
    regexp = "Using the variable\\(s\\)"
  )
  expect_equal(x$get("name"), "test")

  dat$panel2 <- dat$panel
  suppressMessages(expect_message(
    trelliscope(dat, name = "test"),
    regexp = "Found multiple columns"
  ))

  dat2 <- dat[, -c(1:2)]
  suppressMessages(expect_error(
    trelliscope(dat2, name = "test"),
    regexp = "Could not find columns of the data that uniquely define each row"
  ))
})
