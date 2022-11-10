dat <- ggplot2::mpg |>
  tidyr::nest(data = !dplyr::one_of(c("manufacturer", "class"))) |>
  dplyr::mutate(panel = map_plot(data, ~
    (ggplot2::ggplot(aes(hwy, cty), data = .x) + ggplot2::geom_point())
  ))

test_that2("trelliscope instantiation", {
  expect_error(
    trelliscope(iris, name = "test"),
    regexp = "that references a plot or image"
  )

  suppressMessages(expect_error(
    trelliscope(dat),
    regexp = "argument \"name\" is missing"
  ))

  suppressMessages(expect_message(
    x <- trelliscope(dat, name = "test"),
    regexp = "Using the variables"
  ))
  expect_equal(x$get("name"), "test")

  dat$panel2 <- dat$panel
  suppressMessages(expect_message(
    trelliscope(dat, name = "test"),
    regexp = "Found multiple columns"
  ))

  dat2 <- dat[, -c(1:2)]
  suppressMessages(expect_error(
    trelliscope(dat2, name = "test"),
    regexp = "Could not find columns of the data that uniquely"
  ))
})
