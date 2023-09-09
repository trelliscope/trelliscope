test_that("server works", {
  # gapo <- gapminder |>
  #   filter(continent == "Oceania")

  # d <- gapo |>
  #   group_by(country) |>
  #   summarise(
  #     mean_lexp = number(mean(life_exp)),
  #     .groups = "drop"
  #   )

  # # lazy panel
  # plot_fn <- function(country) {
  #   x <- filter(gapo, country == {{ country }})
  #   ggplot(x, aes(year, life_exp)) +
  #     geom_point()
  # }

  # d <- d |>
  #   mutate(
  #     lazy_panel = panel_lazy(plot_fn, gapo)
  #   )

  # dt <- as_trelliscope_df(d, name = "gapminder-test") |>
  #   set_panel_options(
  #     lazy_panel = panel_options(prerender = FALSE)
  #   )

  # suppressMessages(write_trelliscope(dt))
  # s <- suppressMessages(start_server(dt))
})
