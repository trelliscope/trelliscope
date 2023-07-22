dat <- (ggplot(aes(hwy, cty), data = mpg) +
  geom_point() +
  facet_panels(~ class + manufacturer)) |>
  as_panels_df(panel_col = "panel") |>
  mutate(
    class2 = factor(class),
    long_tail = 10^seq_len(dplyr::n()) / 1e20
  ) |>
  left_join(
    mpg |>
      group_by(class, manufacturer) |>
      summarise(mean_cty = mean(cty), .groups = "drop"),
    by = c("class", "manufacturer")
  )


x <- as_trelliscope_df(dat, name = "test",
  key_cols = c("manufacturer", "class"))
xo <- get_trobj(x)

test_that2("set_default_layout", {
  expect_error(
    list() |> set_default_layout(),
    regexp = "Expecting a trelliscope data frame"
  )

  b <- x |>
    set_default_layout()
  bo <- get_trobj(b)

  obj <- bo$get("state")$get("layout")
  expect_true(!is.null(obj))
  expect_equal(obj$get("type"), "layout")

  # make sure we haven't changed the underlying object
  expect_null(xo$get("state")$get("layout"))

  expect_message(
    a <- b |>
      set_default_layout(ncol = 3),
    regexp = "Replacing existing layout state specification"
  )
  ao <- get_trobj(a)

  # make sure "b" wasn't changed
  expect_equal(bo$get("state")$get("layout")$get("ncol"), 1)
})

test_that2("set_default_labels", {
  expect_error(
    list() |> set_default_labels("test"),
    regexp = "Expecting a trelliscope data frame"
  )

  b <- x |>
    set_default_labels(varnames = "manufacturer")
  bo <- get_trobj(b)

  obj <- bo$get("state")$get("labels")
  expect_true(!is.null(obj))
  expect_equal(obj$get("type"), "labels")
  expect_equal(obj$get("varnames"), I("manufacturer"))

  # make sure we haven't changed the underlying object
  expect_null(xo$get("state")$get("labels"))

  expect_message(
    a <- b |>
      set_default_labels(varnames = NULL),
    "Replacing existing labels state specification"
  )
  ao <- get_trobj(a)

  expect_length(ao$get("state")$get("labels")$get("varnames"), 0)

  # make sure "b" hasn't been changed
  expect_equal(bo$get("state")$get("labels")$get("varnames"), I("manufacturer"))
})

test_that2("set_default_sort", {
  expect_error(
    list() |> set_default_sort("test"),
    regexp = "Expecting a trelliscope data frame"
  )

  expect_error(
    b <- x |>
      set_default_sort(varnames = c("a", "b"), dirs = rep("asc", 3)),
    regexp = "must have same length"
  )

  b <- x |>
    set_default_sort(varnames = c("manufacturer", "class"),
      dirs = c("asc", "desc"))
  bo <- get_trobj(b)

  obj <- bo$get("state")$get("sort")
  expect_length(obj, 2)
  expect_equal(obj[[1]]$get("type"), "sort")

  # make sure we haven't changed the underlying object
  expect_length(xo$get("state")$get("sort"), 0)

  expect_message(
    a <- b |>
      set_default_sort(varnames = "manufacturer", dirs = "asc", add = TRUE),
    regexp = "Replacing existing sort state specification for variable"
  )
  ao <- get_trobj(a)

  # "class" should now be first
  expect_equal(names(ao$get("state")$get("sort"))[1], "class")

  expect_message(
    b |>
      set_default_sort(varnames = "manufacturer", dirs = "desc"),
    regexp = "Replacing entire existing sort"
  )
})

test_that2("set_default_filters", {
  expect_error(
    list() |> set_default_filters(),
    regexp = "Expecting a trelliscope data frame"
  )

  expect_error(
    b <- x |>
      set_default_filters(
        filter_string("a", values = c("audi", "volkswagen"))
      ),
    regexp = "not found in the dataset"
  )

  expect_error(
    b <- x |>
      set_default_filters(
        filter_string("manufacturer", values = c("a", "b"))
      ),
    regexp = "could not find the value"
  )

  b <- x |>
    set_default_filters(
      filter_string("manufacturer", values = c("audi", "volkswagen")),
      filter_range("mean_cty", min = 20)
    )
  bo <- get_trobj(b)

  obj <- bo$get("state")$get("filter")
  expect_length(obj, 2)
  expect_equal(obj[[1]]$get("type"), "filter")

  # make sure we haven't changed the underlying object
  expect_length(xo$get("state")$get("filter"), 0)

  expect_message(
    a <- b |>
      set_default_filters(
        filter_string("manufacturer", regexp = "for")
      ),
    regexp = "Replacing existing filter state specification for variable"
  )
  ao <- get_trobj(a)

  # "mean_cty" should now be first
  expect_equal(names(ao$get("state")$get("filter"))[1], "mean_cty")

  expect_message(
    b |>
      set_default_filters(
        filter_string("manufacturer", regexp = "for"),
        add = FALSE
      ),
    regexp = "Replacing entire existing filter"
  )
})

test_that2("add_view", {
  expect_error(
    list() |> add_view("test view", a = 1),
    regexp = "Expecting a trelliscope data frame"
  )

  expect_error(
    x |> add_view("test view", a = 1),
    regexp = "Expecting a trelliscope state definition"
  )

  b <- x |>
    add_view(
      name = "test view",
      state_layout(ncol = 5),
      state_labels(c("manufacturer", "class")),
      state_sort("manufacturer"),
      state_sort("mean_cty", "desc"),
      filter_string("manufacturer", values = c("audi", "volkswagen")),
      filter_range("mean_cty", min = 10)
    )
  bo <- get_trobj(b)

  expect_equal(names(bo$get("views")), "test view")

  # make sure we haven't changed the underlying object
  expect_length(xo$get("views"), 0)

  expect_message(
    x |> add_view(name = "test view", state_layout(), state_layout()),
    "Multiple layout definitions"
  ) |> suppressMessages()
  expect_message(
    x |> add_view(name = "test view", state_labels(), state_labels()),
    "Multiple labels definitions"
  ) |> suppressMessages()

  expect_message(
    b |> add_view("test view"),
    regexp = "Overwriting view 'test view'"
  ) |> suppressMessages()
})

test_that2("input pipe functions", {
  b <- x |>
    add_inputs(
      input_radio(name = "good_radio",
        label = "Is it good?", options = c("no", "yes")),
      input_checkbox(name = "good_checkbox",
        label = "Is it good?", options = c("no", "yes")),
      input_select(name = "good_select",
        label = "Is it good?", options = c("no", "yes")),
      input_multiselect(name = "good_multiselect",
        label = "Is it good?", options = c("no", "yes")),
      input_text(name = "opinion", label = "What do you think?",
        height = 6),
      input_number(name = "rank", label = "Rank this panel"),
      email = "a@b.com",
      vars = "class"
    )
  bo <- get_trobj(b)

  expect_length(bo$get("inputs")$get("inputs"), 6)
  expect_length(xo$get("inputs"), 0)
  for (inpt in bo$get("inputs")$get("inputs"))
    expect_s3_class(inpt, "trelliscope_input_def")

  expect_message(
    b |>
      add_inputs(
        input_radio(name = "good_radio", options = 1:5),
        email = "a@b.com",
        vars = "class"
      ),
    regexp = "Overwriting input 'good_radio'"
  )
})

test_that2("as_json", {
  expect_error(
    as_json(x),
    regexp = NA # no error
  ) |> suppressMessages()
})
