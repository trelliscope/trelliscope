plotdir <- tempfile()
dir.create(plotdir)
mpg2 <- dplyr::filter(ggplot2::mpg, manufacturer == "volkswagen")
get_x_range <- function(obj)
  obj$scales$get_scales("x")$limits
  # ggplot_build(obj)$layout$panel_scales_x[[1]]$range$range
get_y_range <- function(obj)
  obj$scales$get_scales("y")$limits
  # ggplot_build(obj)$layout$panel_scales_y[[1]]$range$range

test_that2("facet_trellisope", {
  a <- ggplot(aes(hwy, cty), data = mpg2) + geom_point() +
    facet_panels(~ manufacturer + class)
  expect_s3_class(a, c("facet_panels", "ggplot"))

  a1 <- as_trelliscope_df(a, name = "mpg1", path = plotdir)
  expect_s3_class(a1, "data.frame")
  expect_s3_class(a1, "trelliscope")
  a2 <- suppressMessages(write_trelliscope(a1))
  expect_true(dir.exists(file.path(plotdir, "displays/mpg1")))
  expect_true(file.exists(file.path(plotdir,
    "displays/mpg1/displayInfo.jsonp")))

  a4 <- (ggplot(aes(hwy, cty), data = mpg2) + geom_point() +
    facet_panels(~ manufacturer + class)) |>
    as_panels_df(as_plotly = TRUE, plotly_cfg = list(displaylogo = FALSE))
  expect_s3_class(a4, "data.frame")
  expect_s3_class(a4$panel, "ggpanel_vec")

  a5 <- suppressMessages(as_trelliscope_df(a4, name = "mpg2", path = plotdir) |>
    write_trelliscope())
  dl <- read_json_p(file.path(plotdir, "displays/displayList.jsonp"))
  expect_true(nrow(dl) == 2)

  # panels auto-build if printed
  a55 <- (ggplot(aes(hwy, cty), data = mpg2) + geom_point() +
    facet_panels(~ manufacturer + class))
  a55df <- suppressMessages(print(a55, view = FALSE))
  expect_s3_class(a55df, "trelliscope")
  expect_true(length(list.files(get_trobj(a55df)$path)) > 0)

  # panels auto-build if needed
  a6 <- (ggplot(aes(hwy, cty), data = mpg2) + geom_point() +
    facet_panels(~ manufacturer + class)) |>
    as_trelliscope_df(name = "mpg3", path = plotdir)

  # write pngs (with a blank panel)
  b <- as_panels_df(a)
  expect_error(b$panel[[1]] <- "asdf", "Can't convert")

  tmp <- b$panel[2:3]
  expect_s3_class(tmp, "ggpanel_vec")
})

test_that2("facet_trellisope scales", {
  a7 <- (ggplot(aes(hwy, cty), data = mpg2) + geom_point() +
    facet_panels(~ manufacturer + class, scales = c("free", "same"))) |>
    as_panels_df()
  expect_null(get_x_range(get_panel(a7$panel[[1]])))
  expect_null(get_x_range(get_panel(a7$panel[[2]])))
  expect_equal(
    get_y_range(get_panel(a7$panel[[1]])),
    get_y_range(get_panel(a7$panel[[2]]))
  )
  a8 <- (ggplot(aes(hwy, cty), data = mpg2) + geom_point() +
    facet_panels(~ manufacturer + class, scales = "sliced")) |>
    as_panels_df()
  expect_equal(
    # note: the first panel has the widest range so it doesn't have
    # a scale explicitly assigned to it...
    diff(get_x_range(get_panel(a8$panel[[2]]))),
    diff(get_x_range(get_panel(a8$panel[[3]])))
  )
  expect_equal(
    diff(get_y_range(get_panel(a8$panel[[2]]))),
    diff(get_y_range(get_panel(a8$panel[[3]])))
  )

  expect_error(
    (ggplot(aes(hwy, cty), data = mpg2) + geom_point() +
      facet_panels(~ manufacturer + class, scales = "stuff")) |>
      as_panels_df(),
    "may only be one of the"
  )

  a9 <- (ggplot(aes(model, trans), data = mpg2) + geom_point() +
    facet_panels(~ manufacturer + class, scales = c("free", "same"))) |>
    as_panels_df()
  expect_null(get_x_range(get_panel(a9$panel[[1]])))
  expect_null(get_x_range(get_panel(a9$panel[[2]])))
  expect_equal(
    get_y_range(get_panel(a9$panel[[1]])),
    get_y_range(get_panel(a9$panel[[2]]))
  )

  expect_message(
    (ggplot(aes(model, trans), data = mpg2) + geom_point() +
      facet_panels(~ manufacturer + class,
        scales = c("sliced", "same"))) |>
      as_panels_df(),
    "does not know how to handle"
  )
})

test_that2("facet_trellisope errors/warnings", {
  expect_error(as_panels_df(iris), "only works with ggplot objects")

  expect_error(
    (ggplot(mapping = aes(hwy, cty)) + geom_line(data = mpg2) +
      facet_panels(~ z)) |>
      as_panels_df(),
    "All facet_panels facet columns must be found"
  )

  dd <- mpg2
  dd$pnl <- 1
  dd$dat <- 1
  dd$xtra <- 1
  dd$xtra2 <- rnorm(nrow(dd))

  a <- ggplot(aes(hwy, cty), data = dd) + geom_point() +
    facet_panels(~ manufacturer + class)

  expect_warning(
    as_panels_df(a, panel_col = "pnl",
      unnest_cols = c("pnl", "xtra")),
    "A variable with name matching panel_col"
  )

  expect_error(
    as_panels_df(a, unnest_cols = c("xtra2")),
    "distinct within the values"
  )

  expect_error(
    as_panels_df(a, panel_col = "manufacturer"),
    "one of the facet columns"
  )
})


  # iris_imgs <- data.frame(
  #   Species = c("setosa", "versicolor", "virginica"),
  #   img = c(
  #     "https://upload.wikimedia.org/wikipedia/commons/a/a7/Irissetosa1.jpg",
  #     "https://upload.wikimedia.org/wikipedia/commons/2/27/Blue_Flag%2C_Ottawa.jpg",
  #     "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f8/Iris_virginica_2.jpg/1920px-Iris_virginica_2.jpg"
  #   )
  # )

  # iris |>
  #   nest(data = !Species) |>
  #   left_join(iris_imgs, by = "Species") |>
  #   mutate(img = img_panel(img)) |>
  #   as_trelliscope_df("iris img", path = plotdir) |>
  #   write_trelliscope()

  # panel_path <- "/tmp/test1/displays/iris_img2/panels"
  # dir.create(panel_path, recursive = TRUE, showWarnings = FALSE)
  # for (i in seq_len(nrow(iris_imgs))) {
  #   out <- file.path(panel_path, paste0(iris_imgs$Species[i], ".jpg"))
  #   download.file(iris_imgs$img[i], destfile = out)
  # }
  # list.files(panel_path)

  # iris |>
  #   nest(data = !Species) |>
  #   mutate(img = img_panel_local(paste0(Species, ".jpg"))) |>
  #   as_trelliscope_df("iris img2", path = plotdir) |>
  #   write_trelliscope()
  # # TODO: make sure this checks that the panels are there...

  # # TODO: iframe examples


  # dd <- mpg
  # dd$pnl <- 1
  # dd$dat <- 1
  # dd$xtra <- 1
  # dd$xtra2 <- rnorm(nrow(dd))

  # b1 <- as_panels_df(a, panel_col = "pnl", data_col = "dat",
  #   unnest_cols = c("pnl", "xtra"))

  # b2 <- as_panels_df(a, as_plotly = TRUE) |>
  #   as_trelliscope_df(name = "mpg", path = "/tmp/test2") |>
  #   write_panels()
  # b2$panels_written
  # list.files(file.path(b2$path, "displays", "mpg", "panels"))

  # b3 <- as_panels_df(a) |>
  #   as_trelliscope_df(name = "mpg", path = "/tmp/test3") |>
  #   write_panels()
  # b3$panels_written
  # list.files(file.path(b3$path, "displays", "mpg", "panels"))

  # b4 <- as_panels_df(a) |>
  #   as_trelliscope_df(name = "mpg", path = "/tmp/test4") |>
  #   write_panels(format = "svg")
  # b4$panels_written
  # list.files(file.path(b4$path, "displays", "mpg", "panels"))




  # # should error
  # b3 <- as_panels_df(a, unnest_cols = c("xtra2"))

  # b4 <- ggplot2::mpg |>
  #   tidyr::nest(data = !dplyr::one_of(c("manufacturer", "class"))) |>
  #   dplyr::mutate(panel = map_plot(data, function(x) {
  #     ggplot2::ggplot(aes(hwy, cty), data = x) geom_point() +
  #   }))
