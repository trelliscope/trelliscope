load_all()

cents <- readr::read_csv("https://gist.githubusercontent.com/tadast/8827699/raw/f5cac3d42d16b78348610fc4ec301e9234f82821/countries_codes_and_coordinates.csv") |>
  select(c(1, 2, 5, 6))
names(cents) <- c("country", "iso_alpha2", "lat", "lon")

readr::write_csv(cents, "data-raw/country-centroids.csv")

cents2 <- select(cents, -country)
cents2 <- cents2[!duplicated(cents2), ]

gap <- gapminder::gapminder |>
  dplyr::left_join(gapminder::country_codes, by = "country") |>
  dplyr::left_join(
    readr::read_csv("data-raw/iso3to2.csv", na = ""), by = "iso_alpha") |>
  dplyr::select(-c(iso_alpha, iso_num)) |>
  dplyr::left_join(cents2, by = "iso_alpha2") |>
  dplyr::rename(life_exp = lifeExp, gdp_percap = gdpPercap)

use_data(gap, overwrite = TRUE)
