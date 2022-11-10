dat <- iris
sq <- seq_len(nrow(dat))
dat$id <- as.character(sq)
dat$letters <- rep(letters, 10)[1:150]
dat$date <- Sys.Date() + 1:150
dat$date2 <- as.character(dat$date)
dat$datetime <- as.POSIXct(dat$date)
tmp <- matrix(c(
  c(2:150, 1),
  c(3:150, 1:2)
), ncol = 2)
dat$lst <- apply(tmp, 1, as.list)
dat$lat <- runif(150, -90, 90)
dat$long <- runif(150, 0, 180)
dat$href <- "https://google.com"
dat$plot <- "a.png"
dat$plot <- img_panel(dat$plot)
dat$plot2 <- img_panel_local(dat$plot)
attr(dat$date2, "label") <- "test date label with label attribute"
# dat$bad <- hms::as_hms(1)

suppressMessages(
  x <- trelliscope(dat, name = "test") |>
    add_meta_labels(id = "test id label with add_meta_labels")
)

test_that2("infer", {
  expect_message(
    infer(x),
    regexp = "No \"layout\" definition supplied"
  ) |> suppressMessages()

  expect_message(
    infer(x),
    regexp = "No \"labels\" definition supplied"
  ) |> suppressMessages()

  b <- x |>
    add_view(name = "view 1")

  expect_message(
    a <- infer(b),
    regexp = "No \"layout\" definition supplied for view"
  ) |> suppressMessages()

  expect_message(
    infer(b),
    regexp = "No \"labels\" definition supplied for view"
  ) |> suppressMessages()

  expect_equal(
    a$get("views")[[1]]$get("state")$get("labels")$get("type"),
    "labels"
  )
  expect_equal(
    a$get("views")[[1]]$get("state")$get("layout")$get("type"),
    "layout"
  )
  expect_equal(
    b$get("views")[[1]]$get("state")$get("labels"),
    NULL
  )
  expect_equal(
    b$get("views")[[1]]$get("state")$get("layout"),
    NULL
  )
})

test_that2("meta variable inference", {
  b <- x |>
    add_meta_def(meta_string("letters", label = "test label with meta_string"))

  # expect_message(
  #   d <- infer_meta(b),
  #   regexp = "Cannot find a data type for variable"
  # ) |> suppressMessages()

  expect_message(
    d <- infer_meta(b),
    regexp = "Meta definitions inferred"
  ) |> suppressMessages()

  # make sure b didn't get updated
  expect_length(b$get("metas"), 1)

  expect_length(d$get("metas"), ncol(dat) - 1)
  expect_length(d$df, ncol(dat)) # the "bad" columns shouldn't be removed
  expect_equal(d$df_cols_ignore, "lst")

  metas <- d$get("metas")
  lbls <- lapply(metas, function(x) x$get("label"))

  expect_length(unlist(lbls), ncol(dat) - 1)
  expect_equal(lbls$id, "test id label with add_meta_labels")
  expect_equal(lbls$date2, "test date label with label attribute")
  expect_equal(lbls$letters, "test label with meta_string")
})
