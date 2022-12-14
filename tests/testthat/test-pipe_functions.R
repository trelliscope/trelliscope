dat <- ggplot2::mpg |>
  tidyr::nest(data = !dplyr::one_of(c("manufacturer", "class"))) |>
  dplyr::mutate(
    mean_cty = purrr::map_dbl(data, function(x) mean(x$cty)),
    panel = map_plot(data, ~
      (ggplot2::ggplot(aes(hwy, cty), data = .x)) + geom_point()),
    class2 = factor(class)
  )

x <- trelliscope(dat, name = "test", key_cols = c("manufacturer", "class"))

test_that2("add_meta_def", {
  expect_error(
    list() |> add_meta_def(meta_string("test")),
    regexp = "Expecting a trelliscope display object"
  )

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

# if we don't specify factor levels, it will infer them
test_that2("meta factor levels inference", {
  b <- x |>
    add_meta_def(meta_factor("manufacturer", "vehicle manufacturer"))
  expect_equal(
    b$get("metas")$manufacturer$get("levels"),
    sort(unique(dat$manufacturer))
  )

  b <- x |>
    add_meta_def(meta_factor("class2", "vehicle class (as factor)"))
  expect_equal(
    b$get("metas")$class2$get("levels"),
    levels(dat$class2)
  )
})

test_that2("add_meta_defs", {
  expect_error(
    list() |> add_meta_defs(),
    regexp = "Expecting a trelliscope display object"
  )

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

test_that2("add_meta_labels", {
  expect_error(
    list() |> add_meta_defs(),
    regexp = "Expecting a trelliscope display object"
  )

  b <- x |>
    add_meta_labels(manufacturer = "test manufacturer")

  expect_equal(b$meta_labels, list(manufacturer = "test manufacturer"))
  expect_length(x$meta_labels, 0)
})

test_that2("set_default_layout", {
  expect_error(
    list() |> set_default_layout(),
    regexp = "Expecting a trelliscope display object"
  )

  b <- x |>
    set_default_layout()
  obj <- b$get("state")$get("layout")
  expect_true(!is.null(obj))
  expect_equal(obj$get("type"), "layout")

  # make sure we haven't changed the underlying object
  expect_null(x$get("state")$get("layout"))

  expect_message(
    a <- b |>
      set_default_layout(nrow = 3, ncol = 3),
    regexp = "Replacing existing layout state specification"
  )

  expect_equal(a$get("state")$get("layout")$get("nrow"), 3)

  # make sure "b" wasn't changed
  expect_equal(b$get("state")$get("layout")$get("nrow"), 1)
})

test_that2("set_default_labels", {
  expect_error(
    list() |> set_default_labels("test"),
    regexp = "Expecting a trelliscope display object"
  )

  b <- x |>
    set_default_labels(varnames = "manufacturer")

  obj <- b$get("state")$get("labels")
  expect_true(!is.null(obj))
  expect_equal(obj$get("type"), "labels")
  expect_equal(obj$get("varnames"), I("manufacturer"))

  # make sure we haven't changed the underlying object
  expect_null(x$get("state")$get("labels"))

  expect_message(
    a <- b |>
      set_default_labels(varnames = NULL),
    "Replacing existing labels state specification"
  )

  expect_length(a$get("state")$get("labels")$get("varnames"), 0)

  # make sure "b" hasn't been changed
  expect_equal(b$get("state")$get("labels")$get("varnames"), I("manufacturer"))
})

test_that2("set_default_sort", {
  expect_error(
    list() |> set_default_sort("test"),
    regexp = "Expecting a trelliscope display object"
  )

  expect_error(
    b <- x |>
      set_default_sort(varnames = c("a", "b"), dirs = rep("asc", 3)),
    regexp = "must have same length"
  )

  b <- x |>
    set_default_sort(varnames = c("manufacturer", "class"), dirs = c("asc", "desc"))

  obj <- b$get("state")$get("sort")
  expect_length(obj, 2)
  expect_equal(obj[[1]]$get("type"), "sort")

  # make sure we haven't changed the underlying object
  expect_length(x$get("state")$get("sort"), 0)

  expect_message(
    a <- b |>
      set_default_sort(varnames = "manufacturer", dirs = "asc", add = TRUE),
    regexp = "Replacing existing sort state specification for variable"
  )

  # "class" should now be first
  expect_equal(names(a$get("state")$get("sort"))[1], "class")

  expect_message(
    b |>
      set_default_sort(varnames = "manufacturer", dirs = "desc"),
    regexp = "Replacing entire existing sort"
  )
})

test_that2("set_default_filters", {
  expect_error(
    list() |> set_default_filters(),
    regexp = "Expecting a trelliscope display object"
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

  obj <- b$get("state")$get("filter")
  expect_length(obj, 2)
  expect_equal(obj[[1]]$get("type"), "filter")

  # make sure we haven't changed the underlying object
  expect_length(x$get("state")$get("filter"), 0)

  expect_message(
    a <- b |>
      set_default_filters(
        filter_string("manufacturer", regexp = "for")
      ),
    regexp = "Replacing existing filter state specification for variable"
  )

  # "mean_cty" should now be first
  expect_equal(names(a$get("state")$get("filter"))[1], "mean_cty")

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
    regexp = "Expecting a trelliscope display object"
  )

  expect_error(
    x |> add_view("test view", a = 1),
    regexp = "Expecting a trelliscope state definition"
  )

  b <- x |>
    add_view(
      name = "test view",
      state_layout(nrow = 3, ncol = 5),
      state_labels(c("manufacturer", "class")),
      state_sort("manufacturer"),
      state_sort("mean_cty", "desc"),
      filter_string("manufacturer", values = c("audi", "volkswagen")),
      filter_range("mean_cty", min = 10)
    )

  expect_equal(names(b$get("views")), "test view")

  # make sure we haven't changed the underlying object
  expect_length(x$get("views"), 0)

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
        width = 100, height = 6),
      input_number(name = "rank", label = "Rank this panel")
    )

  expect_length(b$get("inputs"), 6)
  expect_length(x$get("inputs"), 0)
  for (inpt in b$get("inputs"))
    expect_s3_class(inpt, "trelliscope_input_def")

  expect_message(
    b |>
      add_inputs(input_radio(name = "good_radio", options = 1:5)),
    regexp = "Overwriting input 'good_radio'"
  )
})

test_that2("as_json", {
  expect_error(
    as_json(x),
    regexp = NA # no error
  ) |> suppressMessages()
})
