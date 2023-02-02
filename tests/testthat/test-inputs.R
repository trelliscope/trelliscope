test_that("adding inputs", {
  dat <- ggplot2::mpg |>
    tidyr::nest(data = !dplyr::one_of(c("manufacturer", "class"))) |>
    dplyr::mutate(
      panel = map_plot(data, ~
        (ggplot2::ggplot(aes(hwy, cty), data = .x)) + geom_point()),
      class2 = factor(class)
    )

  x <- trelliscope(dat, name = "test", key_cols = c("manufacturer", "class")) |>
    add_inputs(
      input_text(name = "comments", label = "Comments about this panel",
        width = 100, height = 6),
      input_radio(name = "looks_correct",
        label = "Does the data look correct?", options = c("no", "yes"))
    )

  x2 <- x %>%
    add_input_email("johndoe123@fakemail.com")

  expect_error(x$as_json(), "provide a feedback email")

  expect_equal(x2$as_list()$inputs$feedbackInterface$feedbackEmail,
    "johndoe123@fakemail.com")

  expect_error(add_input_vars(x, "asdf"), "can only be valid meta variables")

  expect_no_error(add_input_vars(x, "class"))
})
