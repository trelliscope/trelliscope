load_all()

gapminder$iso_alpha2 <- NULL
gapminder <- gapminder %>%
  dplyr::left_join(gapminder::country_codes, by = "country") %>%
  dplyr::left_join(
    readr::read_csv("data-raw/iso3to2.csv", na = ""), by = "iso_alpha") %>%
  dplyr::select(-c(iso_alpha, iso_num))

use_data(gapminder, overwrite = TRUE)
