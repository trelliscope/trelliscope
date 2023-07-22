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
dat$plot <- panel_local(dat$plot)
attr(dat$date2, "label") <- "test date label with label attribute"
# dat$bad <- hms::as_hms(1)

suppressMessages(suppressWarnings(
  x <- as_trelliscope_df(dat, name = "test")
))
xo <- get_trobj(x)

test_that2("infer", {
  expect_message(
    infer(x),
    regexp = "No default \"layout\" state supplied"
  ) |> suppressMessages()

  expect_message(
    infer(x),
    regexp = "No default \"labels\" state supplied"
  ) |> suppressMessages()

  b <- x |>
    add_view(name = "view 1")

  expect_message(
    a <- infer(b),
    regexp = "No default \"layout\" state supplied for view"
  ) |> suppressMessages()

  expect_message(
    infer(b),
    regexp = "No default \"labels\" state supplied for view"
  ) |> suppressMessages()

  ao <- get_trobj(a)
  bo <- get_trobj(b)
  expect_equal(
    ao$get("views")[[1]]$get("state")$get("labels")$get("type"),
    "labels"
  )
  expect_equal(
    ao$get("views")[[1]]$get("state")$get("layout")$get("type"),
    "layout"
  )
  expect_equal(
    bo$get("views")[[1]]$get("state")$get("labels"),
    NULL
  )
  expect_equal(
    bo$get("views")[[1]]$get("state")$get("layout"),
    NULL
  )
})
