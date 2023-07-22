dat <- (ggplot(aes(hwy, cty), data = mpg) +
  geom_point() +
  facet_panels(~ class + manufacturer)) |>
  as_panels_df(panel_col = "panel")
mpg2 <- filter(mpg, manufacturer == "volkswagen")
plotdir <- tempfile()

test_that2("trelliscope instantiation", {
  expect_error(
    as_trelliscope_df(iris, name = "test"),
    regexp = "uniquely define each row"
  )

  # TODO:
  # suppressMessages(expect_message(
  #   x <- as_trelliscope_df(dat, name = "test"),
  #   regexp = "Using the variables"
  # ))
  x <- as_trelliscope_df(dat, name = "test")
  xo <- get_trobj(x)
  expect_equal(xo$get("name"), "test")

  suppressMessages(expect_message(
    show_info(x),
    "Key columns"
  ))

  # TODO:
  # dat2 <- dat[, -c(1:2)]
  # suppressMessages(expect_error(
  #   as_trelliscope_df(dat2, name = "test"),
  #   regexp = "Could not find columns of the data that uniquely"
  # ))
})

test_that2("trelliscope printing", {
  disp <- (ggplot(aes(hwy, cty), data = mpg2) +
    geom_point() +
    facet_panels(~ class)) |>
    as_panels_df()

  summ <- mpg2 |>
    group_by(class) |>
    summarise(
      mean_cty = mean(cty),
      min_cty = min(cty),
      wiki_link = paste0("https://en.wikipedia.org/wiki/", class[1])
    )

  disp <- disp |>
    left_join(summ, by = "class") |>
    as_trelliscope_df(name = "mpg", path = plotdir)

  expect_true(TRUE)
})

# TODO: test keysig:
# - If it is explicitly specified in as_trelliscope_df(), make sure it isn't overridden
# - Make sure it is set even when not calling write_trelliscope or panels already written

# TODO: once print method reflects this info, test it
# disp <- disp |>
#   set_default_labels(c("class", "mean_cty"))

# disp <- disp |>
#   set_default_layout(ncol = 5)

# disp <- disp |>
#   set_default_sort(c("class", "mean_cty"), dir = c("asc", "desc"))

# disp <- disp |>
#   set_default_filters(
#     filter_string("class", values = "compact"),
#     filter_range("mean_cty", max = 22)
#   )

# disp <- disp |>
#   add_view(
#     name = "Classes that have vehicles with less than 20 city mpg",
#     filter_range("min_cty", max = 20),
#     state_sort("min_cty", dir = "desc")
#   )

# disp <- disp |>
#   add_inputs(
#     input_text(name = "comments", label = "Comments about this panel",
#       width = 100, height = 6),
#     input_radio(name = "looks_correct",
#       label = "Does the data look correct?", options = c("no", "yes"))
#   )
