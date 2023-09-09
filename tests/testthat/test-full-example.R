test_that("full example runs without issue", {
  # summarize gap by country
  gapo <- gap |>
    filter(continent == "Oceania") |>
    droplevels()

  d <- gapo |>
    arrange(year) |>
    group_by(country, continent, iso_alpha2) |>
    mutate(pct_chg = 100 * (life_exp - lag(life_exp)) / lag(life_exp)) |>
    summarise(
      mean_lexp = number(mean(life_exp)),
      mean_gdp = currency(mean(gdp_percap)),
      max_lexp_pct_chg = max(pct_chg, na.rm = TRUE),
      dt_lexp_max_pct_chg = as.Date(paste0(year[which.max(pct_chg)], "-01-01")),
      dttm_lexp_max_pct_chg = as.POSIXct(dt_lexp_max_pct_chg),
      wiki_link = href(paste0("https://en.wikipedia.org/wiki/", country[1])),
      .groups = "drop"
    )

  # add some ggplots
  p <- ggplot(gapo, aes(year, life_exp)) +
    geom_point() +
    facet_panels(vars(country, continent))

  d1 <- as_panels_df(p, panel_col = "lexp_time")
  d2 <- as_panels_df(p, panel_col = "lexp_time_pl", as_plotly = TRUE)

  # join with our main dataset
  d <- left_join(d, d1, by = c("country", "continent"))
  d <- left_join(d, d2, by = c("country", "continent"))

  # lazy panel
  plot_fn <- function(country) {
    x <- filter(gapo, country == {{ country }})
    ggplot(x, aes(year, life_exp)) +
      geom_point()
  }

  d <- d |>
    mutate(
      lazy_panel = panel_lazy(plot_fn)
    )

  # also add panel_local and panel_url columns for flags
  # flag2 is also added not explicitly as a panel column
  flags_dir <- "test-data"
  flag_base_url <- "https://raw.githubusercontent.com/hafen/countryflags/master/png/512/"

  d <- mutate(d,
    flag = panel_local(file.path(flags_dir, paste0(iso_alpha2, ".png"))),
    flag_url = panel_url(paste0(flag_base_url, iso_alpha2, ".png"))
  )

  # now set variable labels and tags
  # (this could be done after it's a trelliscope df as well)
  d <- d |>
    set_var_labels(
      mean_lexp = "Mean life expectancy",
      mean_gdp = "Mean GDP per capita",
      max_lexp_pct_chg = "Max % year-to-year change in life expectancy",
      dt_lexp_max_pct_chg = "Date of max % year-to-year change in life expectancy",
      dttm_lexp_max_pct_chg = "Date-time of max % year-to-year change in life expectancy",
      wiki_link = "Link to country Wikipedia entry",
      flag = "Flag",
      flag_url = "Flag URL"
    )

  d <- d |>
    set_tags(
      stats = c("mean_lexp", "mean_gdp", "max_lexp_pct_chg"),
      info = c("country", "continent", "iso_alpha2")
    )

  print(select(d, lexp_time, lexp_time_pl, lazy_panel,
    flag, flag_url))

  expect_warning(
    dt <- suppressMessages(as_trelliscope_df(d, name = "gap")),
    "are not in the correct location"
  )

  # set panel options (if not specified, defaults are used)
  dt <- dt |>
    set_panel_options(
      lexp_time = panel_options(width = 600, height = 400, format = "svg")
    ) |>
    set_primary_panel("lexp_time_pl")

  # now set all the other stuff...
  dt <- dt |>
    set_default_labels(c("country", "continent", "wiki_link")) |>
    set_default_layout(ncol = 4) |>
    set_default_sort(c("continent", "mean_lexp"), dir = c("asc", "desc")) |>
    set_default_filters(
      filter_string("continent", values = "Oceania"),
      filter_range("mean_lexp", min = 50)
    ) |>
    add_view(
      name = "Countries with high life expectancy (mean >= 60)",
      filter_range("mean_lexp", min = 60),
      state_sort("mean_gdp", dir = "desc")
    ) |>
    add_inputs(
      input_text(name = "comments", label = "Comments about this panel",
        height = 6),
      input_radio(name = "looks_correct",
        label = "Does the data look correct?", options = c("no", "yes")),
      email = "johndoe123@fakemail.net"
    )

  print(dt)

  suppressMessages(view_trelliscope(dt))
})
