test_that("adding inputs", {
  dat <- (ggplot(aes(hwy, cty), data = mpg) +
    geom_point() +
    facet_panels(~ class + manufacturer)) |>
    as_panels_df(panel_col = "panel") %>%
    mutate(class2 = factor(class))

  x <- as_trelliscope_df(dat, name = "test", key_cols = c("manufacturer", "class")) |>
    add_inputs(
      input_text(name = "comments", label = "Comments about this panel",
        height = 6),
      input_radio(name = "looks_correct",
        label = "Does the data look correct?", options = c("no", "yes")),
      email = "johndoe123@fakemail.com",
      vars = "class"
    )

  expect_equal(get_trobj(x)$as_list()$inputs$feedbackInterface$feedbackEmail,
    "johndoe123@fakemail.com")
  expect_equal(get_trobj(x)$as_list()$inputs$feedbackInterface$includeMetaVars,
    I("class"))

  expect_error(as_trelliscope_df(dat, name = "test", key_cols = c("manufacturer", "class")) |>
    add_inputs(
      input_text(name = "comments", label = "Comments about this panel",
        height = 6),
      email = "johndoe123@fakemail.com",
      vars = "asdf"
    ),
    "can only be valid meta variables")
})
