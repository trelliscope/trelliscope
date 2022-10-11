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

  expect_error(
    trelliscope(dat),
    regexp = "argument \"name\" is missing"
  )

  x <- trelliscope(dat, name = "test")
  expect_equal(x$get("name"), "test")

  dat$panel2 <- dat$panel
  expect_message(
    trelliscope(dat, name = "test"),
    regexp = "Found multiple columns"
  )
})
