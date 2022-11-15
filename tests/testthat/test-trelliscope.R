dat <- ggplot2::mpg |>
  tidyr::nest(data = !dplyr::one_of(c("manufacturer", "class"))) |>
  dplyr::mutate(panel = map_plot(data, ~
    (ggplot2::ggplot(aes(hwy, cty), data = .x) + ggplot2::geom_point())
  ))
mpg2 <- filter(mpg, manufacturer == "volkswagen")
plotdir <- tempfile()

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

  suppressMessages(expect_message(
    x$print(),
    "Key columns"
  ))

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

test_that2("trelliscope printing", {
  disp <- (ggplot(aes(hwy, cty), data = mpg2) +
    geom_point() +
    facet_trelliscope(~ class)) |>
    build_panels() |>
    mutate(
      mean_cty = purrr::map_dbl(data, ~ mean(.x$cty)),
      min_cty = purrr::map_dbl(data, ~ min(.x$cty)),
      wiki_link = paste0("https://en.wikipedia.org/wiki/", class)
    ) |>
    trelliscope(name = "mpg", path = plotdir)

  suppressMessages(expect_message(
    disp$print(),
    "Panels written: no"
  ))

  disp <- disp |>
    write_panels(width = 800, height = 500, format = "svg")
  suppressMessages(expect_message(
    disp$print(),
    "Panels written: yes"
  ))

  disp <- disp |>
    add_meta_defs(
      meta_number("mean_cty",
        label = "Mean of city miles per gallon",
        digits = 2),
      meta_href("wiki_link", label = "Wikipedia page for vehicle class")
    )
  suppressMessages(expect_message(
    disp$print(),
    "Defined metadata variables"
  ))

  disp <- disp |>
    add_meta_labels(
      min_cty = "Lowest observed city miles per gallon"
    )
  suppressMessages(expect_message(
    disp$print(),
    "Lowest observed"
  ))
})

# TODO: once print method reflects this info, test it
# disp <- disp |>
#   set_labels(c("class", "mean_cty"))

# disp <- disp |>
#   set_layout(nrow = 3, ncol = 5)

# disp <- disp |>
#   set_sort(c("class", "mean_cty"), dir = c("asc", "desc"))

# disp <- disp |>
#   set_filters(
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



